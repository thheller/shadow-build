(ns shadow.cljs.build
  (:import [java.io File StringWriter FileOutputStream FileInputStream StringReader PushbackReader]
           [java.net URL]
           [com.google.javascript.jscomp JSModule SourceFile SourceFile$Generated SourceFile$Generator SourceFile$Builder JSModuleGraph]
           (clojure.lang ExceptionInfo)
           (java.util.jar JarFile JarEntry)
           (com.google.javascript.jscomp.deps JsFileParser)
           (java.util.logging Level))
  (:require [clojure.pprint :refer (pprint)]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [cljs.analyzer :as ana]
            [cljs.closure :as closure]
            [cljs.compiler :as comp]
            [cljs.source-map :as sm]
            [cljs.env :as env]
            [cljs.tagged-literals :as tags]
            [cljs.util :as cljs-util]
            [clojure.core.reducers :as r]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.core.reducers :as r]
            [loom.graph :as lg]
            [loom.alg :as la]
            [cognitect.transit :as transit]
            [clojure.java.shell :as shell]
            [shadow.cljs.util :as util]))

(defn ^com.google.javascript.jscomp.Compiler make-closure-compiler []
  (com.google.javascript.jscomp.Compiler/setLoggingLevel Level/WARNING)
  (doto (com.google.javascript.jscomp.Compiler.)
    ;; the thread lingers and prevents the JVM from exiting
    ;; haven't found a clean way to shut it down otherwise
    ;; but given that only one thread is used to compile anyways there
    ;; is really no gain to running in another thread?
    (.disableThreads)))


(defn write-cache [file data]
  (with-open [out (FileOutputStream. file)]
    (let [w (transit/writer out :json {:handlers {URL (transit/write-handler "url" str)}})]
      (transit/write w data)
      )))

(defn read-cache [file]
  (with-open [in (FileInputStream. file)]
    (let [r (transit/reader in :json {:handlers {"url" (transit/read-handler #(URL. %))}})]
      (transit/read r)
      )))

(comment
  ;; maybe add some kind of type info later
  (def a-resource
    '{:input deref
      :output string
      :type (either :js :cljs)
      :jar boolean
      :last-modified long
      :requires #{name}
      :provides #{name}
      ;; only :cljs
      :ns name
      :ns-info ns-info
      }))

(defprotocol BuildLog
  (log-warning [this log-string])
  (log-progress [this log-string])
  (log-time-start [this log-string])
  (log-time-end [this log-string time-in-ms]))

(def ^{:dynamic true} *time-depth* 0)

(defmacro with-logged-time
  [[logger msg] & body]
  `(let [msg# ~msg]
     (log-time-start ~logger msg#)
     (let [start# (System/currentTimeMillis)
           result# (binding [*time-depth* (inc *time-depth*)]
                     ~@body)]
       (log-time-end ~logger msg# (- (System/currentTimeMillis) start#))
       result#)
     ))

(defn compiler-state? [state]
  (true? (::is-compiler-state state)))

(defn classpath-entries
  "finds all js files on the classpath matching the path provided"
  []
  (let [sysp (System/getProperty "java.class.path")]
    (if (.contains sysp ";")
      (str/split sysp #";")
      (str/split sysp #":"))))

(defn usable-resource? [{:keys [type provides requires] :as rc}]
  (or (= :cljs type) ;; cljs is always usable
    (seq provides) ;; provides something is useful
    (seq requires) ;; requires something is less useful?
    (= "goog/base.js" (:name rc)) ;; doesnt provide/require anything but is useful
    ))

(defn is-jar? [^String name]
  (.endsWith (str/lower-case name) ".jar"))

(defn is-cljs-file? [^String name]
  (or (.endsWith (str/lower-case name) ".cljs")
    (.endsWith (str/lower-case name) ".cljc")))

(defn is-js-file? [^String name]
  (.endsWith (str/lower-case name) ".js"))

(defn is-cljs-resource? [^String name]
  (or (is-cljs-file? name)
    (is-js-file? name)
    ))

(defn cljs->js-name [name]
  (str/replace name #"\.cljs$" ".js"))

(defn ns->path [ns]
  (-> ns
      (str)
      (str/replace #"\." "/")
      (str/replace #"-" "_")))

(defn ns->cljs-file [ns]
  (-> ns
      (ns->path)
      (str ".cljs")))

(defn cljs-file->ns [name]
  (-> name
      (str/replace #"\.cljs$" "")
      (str/replace #"_" "-")
      (str/replace #"[/\\]" ".")
      (symbol)))

(defn file-basename [^String path]
  (let [idx (.lastIndexOf path "/")]
    (.substring path (inc idx))
    ))

(defn conj-in [m k v]
  (update-in m k (fn [old] (conj old v))))

(defn set-conj [x y]
  (if x
    (conj x y)
    #{y}))

(defn munge-goog-ns [s]
  (-> s
      (str/replace #"_" "-")
      (symbol)))

(defn list->ns-set [list]
  (->> list
       (reduce (fn [set s]
                 (conj! set (munge-goog-ns s)))
         (transient #{}))
       (persistent!)))

(defn add-goog-dependencies [state {:keys [name input] :as rc}]
  {:pre [(compiler-state? state)]}
  (if (= "goog/base.js" name)
    (assoc rc
      :requires #{}
      :provides #{'goog})
    ;; parse any other js
    (let [deps (-> (JsFileParser. (.getErrorManager (::cc state)))
                   (.parseFile name name @input))]
      (assoc rc
        :requires (list->ns-set (.getRequires deps))
        :provides (list->ns-set (.getProvides deps))))))

(defn requires-from-ns-ast
  [{:keys [emit-constants] :as state} {:keys [name requires uses]}]
  {:pre [(compiler-state? state)]}
  (let [req (set (vals (merge uses requires)))]
    (if (= 'cljs.core name)
      req
      (if emit-constants
        (conj req 'cljs.core 'constants-table 'runtime-setup)
        (conj req 'cljs.core 'runtime-setup)
        ))))

(defn macros-from-ns-ast [state {:keys [require-macros use-macros]}]
  {:pre [(compiler-state? state)]}
  (into #{} (concat (vals require-macros) (vals use-macros))))

(defn update-rc-from-ns [state rc ast]
  {:pre [(compiler-state? state)]}
  (let [ns-name (:name ast)]
    (assoc rc
      :ns ns-name
      :ns-info (dissoc ast :env)
      :provides #{ns-name}
      :macros (macros-from-ns-ast state ast)
      :requires (requires-from-ns-ast state ast))))


(defn peek-into-cljs-resource
  "looks at the first form in a .cljs file, analyzes it if (ns ...) and returns the updated resource
   with ns-related infos"
  [{:keys [logger] :as state} {:keys [name input source-path] :as rc}]
  {:pre [(compiler-state? state)]}
  (let [eof-sentinel (Object.)
        cljc? (.endsWith name ".cljc")
        opts (merge
               {:eof eof-sentinel}
               (when cljc?
                 {:read-cond :allow :features #{:cljs}}))
        rdr (StringReader. @input)
        in (readers/indexing-push-back-reader (PushbackReader. rdr) 1 name)]
    (binding [reader/*data-readers* tags/*cljs-data-readers*]
      (try
        (let [peek (reader/read opts in)]
          (if (identical? peek eof-sentinel)
            (do (log-warning logger (format "File: %s/%s is empty, skipped." source-path name))
                rc)
            (let [ast (util/parse-ns peek)]
              (-> state
                  (update-rc-from-ns rc ast)
                  (assoc :cljc cljc?)))))
        (catch Exception e
          (log-warning logger (format "NS form of %s/%s can't be parsed: %s" source-path name (.getMessage e)))
          (.printStackTrace e)
          rc)))))

(defn inspect-resource
  [state {:keys [name] :as rc}]
  {:pre [(compiler-state? state)]}
  (cond
    (is-js-file? name)
    (->> (assoc rc :type :js :js-name name)
         (add-goog-dependencies state))

    (is-cljs-file? name)
    (let [rc (assoc rc :type :cljs :js-name (str/replace name #"\.clj(s|c)$" ".js"))]
      (if (= name "deps.cljs")
        rc
        (peek-into-cljs-resource state rc)))

    :else
    (throw (ex-info "cannot identify as cljs resource" rc))))

(def ^{:doc "windows filenames need to be normalized because they contain backslashes which browsers don't understand"}
normalize-resource-name
  (if (= File/separatorChar \/)
    identity
    (fn [^String name]
      (str/replace name File/separatorChar \/))))

(defn normalize-foreign-libs [{:keys [foreign-libs externs] :as deps}]
  (if (seq externs)
    ;; FIXME: :externs at top level
    (update-in foreign-libs [0 :externs] concat externs)
    foreign-libs))

(defn should-ignore-resource?
  [{:keys [ignore-patterns] :as state} name]
  (loop [patterns ignore-patterns]
    (if-let [pattern (first patterns)]
      (if (re-find pattern name)
        true
        (recur (rest patterns)))
      false
      )))

(defn create-jar-manifest
  "returns a map of {source-name resource-info}"
  [state path]
  {:pre [(compiler-state? state)]}
  (let [file (io/file path)
        abs-path (.getAbsolutePath file)
        jar-file (JarFile. file)
        last-modified (.lastModified file)
        entries (.entries jar-file)
        slurp-entry (fn [entry]
                      (with-open [in (.getInputStream jar-file entry)]
                        (slurp in)))]
    (loop [result (transient {})]
      (if (not (.hasMoreElements entries))
        (persistent! result)
        (let [^JarEntry jar-entry (.nextElement entries)
              name (.getName jar-entry)]
          (if (or (not (is-cljs-resource? name))
                (should-ignore-resource? state name))
            (recur result)
            (let [url (URL. (str "jar:file:" abs-path "!/" name))
                  rc (inspect-resource state
                       {:name (normalize-resource-name name)
                        :jar true
                        :source-path path
                        :last-modified last-modified
                        :url url
                        :input (atom (slurp-entry jar-entry))})]
              (recur (assoc! result name rc))
              )))))))

(defn write-jar-manifest [file manifest]
  (let [data (->> (vals manifest)
                  ;; :input is non serializable deref, don't want to store actual content
                  ;; might not need it, just a performance issue
                  ;; reading closure jar with js contents 300ms without content 5ms
                  ;; since we are only using a small percentage of those file we delay reading
                  (map #(dissoc % :input))
                  (into []))]
    (write-cache file data)
    ))

(defn read-jar-manifest [file]
  (let [entries (read-cache file)]
    (reduce
      (fn [m {:keys [name url] :as v}]
        (assoc m name (assoc v :input (delay (slurp url)))))
      {}
      entries)))

(defn process-deps-cljs
  [{:keys [use-file-min] :as state} manifest]
  {:pre [(compiler-state? state)
         (map? manifest)]}
  (let [deps (get manifest "deps.cljs")]
    (if (nil? deps)
      manifest
      (let [foreign-libs (-> @(:input deps)
                             (edn/read-string)
                             (normalize-foreign-libs))]

        (reduce
          (fn [result {:keys [externs provides requires] :as foreign-lib}]
            (let [[lib-key lib-other] (cond
                                        (and use-file-min (contains? foreign-lib :file-min))
                                        [:file-min :file]
                                        (:file foreign-lib)
                                        [:file :file-min])
                  lib-name (get foreign-lib lib-key)
                  rc (get result lib-name)]
              (when (nil? rc)
                (throw (ex-info "deps.cljs refers to file not in jar" {:foreign-lib foreign-lib})))

              (let [dissoc-all (fn [m list]
                                 (apply dissoc m list))
                    ;; mark rc as foreign and merge with externs instead of leaving externs as seperate rc
                    rc (assoc rc
                         :foreign true
                         :requires (set (map symbol requires))
                         :provides (set (map symbol provides))
                         :externs-source (->> externs
                                              (map #(get result %))
                                              (map :input)
                                              (map deref)
                                              (str/join "\n")))]
                (-> result
                    (dissoc-all externs)
                    ;; remove :file or :file-min
                    (dissoc (get foreign-lib lib-other))
                    (assoc lib-name rc)))))
          (dissoc manifest "deps.cljs")
          foreign-libs)))))


(defn find-jar-resources
  [{:keys [manifest-cache-dir] :as state} path]
  {:pre [(compiler-state? state)]}
  ;; FIXME: assuming a jar with the same name and same last modified is always identical, probably not. should md5 the full path?
  (let [manifest-name (let [jar (io/file path)]
                        (str (.lastModified jar) "-" (.getName jar) ".manifest"))
        mfile (io/file manifest-cache-dir manifest-name)
        jar-file (io/file path)
        manifest (if (and (.exists mfile)
                       (>= (.lastModified mfile) (.lastModified jar-file)))
                   (read-jar-manifest mfile)
                   (let [manifest (create-jar-manifest state path)]
                     (io/make-parents mfile)
                     (write-jar-manifest mfile manifest)
                     manifest))]
    (-> (process-deps-cljs state manifest)
        (vals))))

(defn make-fs-resource [state source-path rc-name rc-file]
  (inspect-resource
    state
    {:name rc-name
     :file rc-file
     :source-path source-path
     :last-modified (.lastModified rc-file)
     :url (.toURL (.toURI rc-file))
     :input (delay (slurp rc-file))}))

(defn find-fs-resources
  [state ^String path]
  {:pre [(compiler-state? state)
         (seq path)]}
  (let [root (io/file path)
        root-path (.getAbsolutePath root)
        root-len (inc (count root-path))]
    (for [file (file-seq root)
          :let [abs-path (.getAbsolutePath file)]
          :when (and (is-cljs-resource? abs-path)
                  (not (.isHidden file)))
          :let [
                name (-> abs-path
                         (.substring root-len)
                         (normalize-resource-name))]
          :when (not (should-ignore-resource? state name))]

      (make-fs-resource state path name file)
      )))

(defn do-find-resources-in-path [state path]
  {:pre [(compiler-state? state)]}
  (if (.endsWith path ".jar")
    (find-jar-resources state path)
    (find-fs-resources state path)))

(defn- do-find-resources-in-paths [state paths]
  {:pre [(compiler-state? state)]}
  (->> paths
       (mapcat #(do-find-resources-in-path state %))
       (filter usable-resource?)
       (into [])))

(defn get-resource-for-provide [state ns-sym]
  {:pre [(compiler-state? state)
         (symbol? ns-sym)]}
  (when-let [name (get-in state [:provide->source ns-sym])]
    (get-in state [:sources name])))

(defn find-resource-by-js-name [state js-name]
  {:pre [(compiler-state? state)
         (string? js-name)]}
  (let [rcs
        (->> (:sources state)
             (vals)
             (filter #(= js-name (:js-name %)))
             (into []))]
    (when (not= 1 (count rcs))
      ;; FIXME: this should be checked when scanning for resources
      (throw (ex-info (format "multiple resources for js-name:%s" js-name)
               {:js-name js-name
                :resources rcs})))
    (first rcs)))

(defn- get-deps-for-src* [state name]
  {:pre [(compiler-state? state)
         (string? name)]}
  (if (contains? (:deps-visited state) name)
    state
    (let [requires (get-in state [:sources name :requires])]
      (when-not requires
        (throw (ex-info (format "cannot find required deps for \"%s\"" name) {:name name})))

      (let [state (conj-in state [:deps-visited] name)
            state (->> requires
                       (map #(get-in state [:provide->source %]))
                       (into #{})
                       (reduce get-deps-for-src* state))]
        (conj-in state [:deps-ordered] name)
        ))))

(defn get-deps-for-src
  "returns names of all required sources for a given resource by name (in dependency order), does include self
   (eg. [\"goog/string/string.js\" \"cljs/core.cljs\" \"my-ns.cljs\"])"
  [state src-name]
  {:pre [(compiler-state? state)
         (string? src-name)]}
  (-> state
      (assoc :deps-ordered []
             :deps-visited #{})
      (get-deps-for-src* src-name)
      :deps-ordered))

(defn get-deps-for-ns
  "returns names of all required sources for a given ns (in dependency order), does include self
   (eg. [\"goog/string/string.js\" \"cljs/core.cljs\" \"my-ns.cljs\"])"
  [state ns-sym]
  {:pre [(compiler-state? state)
         (symbol? ns-sym)]}
  (let [name (get-in state [:provide->source ns-sym])]
    (when-not name
      (let [reqs (->> state
                      :sources
                      (vals)
                      (filter #(contains? (:requires %) ns-sym))
                      (map :name)
                      (into #{}))]
        (throw (ex-info (format "ns \"%s\" not available, required by %s" ns-sym reqs) {:ns ns-sym :required-by reqs}))))

    (get-deps-for-src state name)
    ))

(defmulti post-analyze
  (fn [ast opts]
    (:op ast))
  :default ::no-op)

(defmethod post-analyze ::no-op [ast opts] ast)

(defmethod post-analyze :ns
  [{:keys [name] :as ast} opts]

  (let [ast (-> ast
                (util/load-macros)
                (util/infer-macro-require)
                (util/infer-macro-use))]
    (util/check-uses! (:env ast) (:uses ast))
    (swap! env/*compiler* assoc-in [::ana/namespaces name] (dissoc ast :env :op :form))
    ast))

(defn hijacked-parse-ns [env form name opts]
  (assoc (util/parse-ns form)
    :env env
    :form form
    :op :ns))

(defn analyze
  [state {:keys [ns name] :as compile-state} form]
  {:pre [(map? compile-state)
         (symbol? ns)
         (string? name)
         (seq name)]}
  ;; (defmulti parse (fn [op & rest] op))
  (let [default-parse ana/parse]
    (binding [*ns* (create-ns ns)
              ana/*passes* [ana/infer-type]
              ;; [infer-type ns-side-effects] is default, we don't want the side effects
              ;; altough it is great that the side effects are now optional
              ;; the default still doesn't handle macros properly
              ;; so we keep hijacking
              ana/*cljs-ns* ns
              ana/*cljs-file* name]

      (with-redefs [ana/parse
                    (fn shadow-parse [op env form name opts]
                      (condp = op
                        ;; the default ana/parse 'ns has way too many side effects we don't need or want
                        ;; don't want analyze-deps -> never needed
                        ;; don't want require or macro ns -> post-analyze
                        ;; don't want check-uses -> doesn't recognize macros
                        ;; don't want check-use-macros -> doesnt handle (:require [some-ns :refer (a-macro)])
                        ;; don't want swap into compiler env -> post-analyze
                        'ns (hijacked-parse-ns env form name opts)
                        (default-parse op env form name opts)))]

        (-> (ana/empty-env) ;; this is anything but empty! requires *cljs-ns*, env/*compiler*
            (ana/analyze form ns state)
            (post-analyze state))))))

(defn do-compile-cljs-string
  [{:keys [name] :as init} reduce-fn cljs-source cljc?]
  (let [eof-sentinel (Object.)
        opts (merge
               {:eof eof-sentinel}
               (when cljc?
                 {:read-cond :allow :features #{:cljs}}))
        in (readers/indexing-push-back-reader (PushbackReader. (StringReader. cljs-source)) 1 name)]

    (binding [comp/*source-map-data* (atom {:source-map (sorted-map)
                                            :gen-col 0
                                            :gen-line 0})]

      (let [result
            (loop [{:keys [ns ns-info] :as compile-state} (assoc init :js "")]
              (let [form (binding [*ns* (create-ns ns)
                                   ana/*cljs-ns* ns
                                   ana/*cljs-file* name
                                   reader/*data-readers* tags/*cljs-data-readers*
                                   reader/*alias-map* (merge reader/*alias-map*
                                                        (:requires ns-info)
                                                        (:require-macros ns-info))]
                           (reader/read opts in))]

                (if (identical? form eof-sentinel)
                  ;; eof
                  compile-state
                  (recur (reduce-fn compile-state form)))))]

        (assoc result :source-map (:source-map @comp/*source-map-data*))
        ))))

(defn default-compile-cljs
  [state compile-state form]
  (let [ast (analyze state compile-state form)
        ast-js (with-out-str
                 (comp/emit ast))

        compile-state (if (= :ns (:op ast))
                        (update-rc-from-ns state compile-state ast)
                        compile-state)]
    (update-in compile-state [:js] str ast-js)
    ))


(defn warning-collector [warnings warning-type env extra]
  (swap! warnings conj {:warning-type warning-type
                        :env env
                        :extra extra}))

(defn warning->msg [{:keys [warning-type env extra] :as warning}]
  (when (contains? ana/*cljs-warnings* warning-type)
    (when-let [s (ana/error-message warning-type extra)]
      (ana/message env s)
      )))

(defmacro with-warnings
  "given a body that produces a compilation result, collect all warnings and assoc into :warnings"
  [& body]
  `(let [warnings# (atom [])
         result# (ana/with-warning-handlers
                   [(partial warning-collector warnings#)]
                   ~@body)]
     (assoc result# :warnings (mapv warning->msg @warnings#))))

(defn compile-cljs-string
  [state cljs-source name cljc?]
  (with-warnings
    (do-compile-cljs-string
      {:name name :ns 'cljs.user}
      (partial default-compile-cljs state)
      cljs-source
      cljc?)))

(defn compile-cljs-seq
  [state cljs-forms name]
  (with-warnings
    (reduce
      (partial default-compile-cljs state)
      {:name name :ns 'cljs.user}
      cljs-forms)))

(defn do-compile-cljs-resource
  "given the compiler state and a cljs resource, compile it and return the updated resource
   should not touch global state"
  [state {:keys [name input cljc] :as rc}]

  (let [source @input]
    (with-logged-time
      [(:logger state) (format "Compile CLJS: \"%s\"" name)]
      (let [{:keys [js ns requires source-map warnings]}
            (cond
              (string? source)
              (compile-cljs-string state source name cljc)
              (vector? source)
              (compile-cljs-seq state source name)
              :else
              (throw (ex-info "invalid cljs source type" {:name name :source source})))]

        (when-not ns
          (throw (ex-info "cljs file did not provide a namespace" {:file name})))

        (assoc rc
          :output js
          :requires requires
          :compiled-at (System/currentTimeMillis)
          :provides #{ns}
          :compiled true
          :warnings warnings
          :source-map source-map)))))

(defn get-cache-file-for-rc
  [{:keys [cache-dir] :as state} {:keys [name] :as rc}]
  (io/file cache-dir "ana" (str name ".cache.transit.json")))

(defn load-cached-cljs-resource
  [{:keys [logger cache-dir] :as state} {:keys [ns js-name name last-modified] :as rc}]
  (let [cache-file (get-cache-file-for-rc state rc)
        cache-js (io/file cache-dir "src" js-name)]

    (when (and (.exists cache-file)
            (> (.lastModified cache-file) last-modified)
            (.exists cache-js)
            (> (.lastModified cache-js) last-modified)

            (let [min-age (->> (get-deps-for-ns state ns)
                               (map #(get-in state [:sources % :last-modified]))
                               #_(map (fn [{:keys [name last-modified] :as src}]
                                        (prn [:last-mod name last-modified])
                                        last-modified))
                               (remove nil?) ;; might not have :last-modified (eg. runtime-setup)
                               (reduce (fn [a b] (Math/max a b))))]
              (> (.lastModified cache-file) min-age)))

      (let [cache-data (read-cache cache-file)]

        (when (= (cljs-util/clojurescript-version) (:version cache-data))
          (log-progress logger (format "Load cached cljs resource \"%s\"" name))

          ;; restore analysis data
          (let [ana-data (:analyzer cache-data)]

            (swap! env/*compiler* assoc-in [::ana/namespaces (:ns cache-data)] ana-data)
            (util/load-macros ana-data))

          ;; merge resource data & return it
          (-> (merge rc cache-data)
              (dissoc :analyzer :version)
              (assoc :output (slurp cache-js))))))))

(defn write-cached-cljs-resource
  [{:keys [logger cache-dir] :as state} {:keys [ns name js-name] :as rc}]

  ;; only cache files that don't have warnings!
  (when-not (seq (:warnings rc))

    (let [cache-file (get-cache-file-for-rc state rc)
          cache-data (-> rc
                         (dissoc :file :output :input :url)
                         (assoc :version (cljs-util/clojurescript-version)
                                :analyzer (get-in @env/*compiler* [::ana/namespaces ns])))
          cache-js (io/file cache-dir "src" js-name)]

      (io/make-parents cache-file)
      (write-cache cache-file cache-data)

      (io/make-parents cache-js)
      (spit cache-js (:output rc))

      (log-progress logger (format "Wrote cache for \"%s\"" name)))))

(defn maybe-compile-cljs
  "take current state and cljs resource to compile
   make sure you are in with-compiler-env"
  [{:keys [cache-dir cache-level] :as state} {:keys [jar] :as src}]
  (let [cache? (and cache-dir
                 ;; i don't trust the register-constant! stuff for now
                 (not (:emit-constants state))
                 (or (= cache-level :all)
                   (and (= cache-level :jars)
                     jar)))]
    (or (when cache?
          (load-cached-cljs-resource state src))
      (let [src (do-compile-cljs-resource state src)]
        (when cache?
          (write-cached-cljs-resource state src))
        src))))

(defn merge-provides [state provided-by provides]
  (reduce
    (fn [state provide]
      (assoc-in state [:provide->source provide] provided-by))
    state
    provides))

(defn unmerge-provides [state provides]
  (reduce
    (fn [state provide]
      (update-in state [:provide->source] dissoc provide))
    state
    provides))

(defn unmerge-resource [state name]
  (if-let [{:keys [provides] :as current} (get-in state [:sources name])]
    (-> state
        (unmerge-provides provides)
        (update-in [:sources] dissoc name))
    ;; else: not present
    state))

(defn valid-resource? [{:keys [type input name provides requires] :as src}]
  (and (contains? #{:js :cljs} type)
    (instance? clojure.lang.IDeref input)
    (string? name)
    (set? provides)
    (set? requires)))

(defn is-cljc? [^String name]
  (.endsWith name ".cljc"))

(defn is-cljs? [^String name]
  (.endsWith name ".cljs"))

(defn merge-resource
  [{:keys [logger] :as state} {:keys [name provides] :as src}]
  (when-not (valid-resource? src)
    (pprint (dissoc src :input))
    (throw (ex-info "not a valid resource" src)))
  (let [cljc? (is-cljc? name)
        cljc-name (when (is-cljs? name)
                    (str/replace name #"cljs$" "cljc"))
        cljs-name (when cljc?
                    (str/replace name #"cljc$" "cljs"))]
    (cond
      ;; don't merge .cljc file if a .cljs of the same name exists
      (and cljc? (contains? (:sources state) cljs-name))
      (do (log-warning logger (format "File conflict: \"%s\" -> \"%s\" (using \"%s\")" name cljs-name cljs-name))
          state)

      ;; if a .cljc exists for a .cljs file unmerge the .cljc and merge the .cljs
      (and (is-cljs? name) (contains? (:sources state) cljc-name))
      (do (log-warning logger (format "File conflict: \"%s\" -> \"%s\" (using \"%s\")" name cljs-name cljs-name))
          (-> state
              (unmerge-resource cljc-name)
              (assoc-in [:sources name] src)
              (merge-provides name provides)))

      :no-conflict
      (-> state
          (unmerge-resource name)
          (assoc-in [:sources name] src)
          (merge-provides name provides)))))

(defn merge-resources [state srcs]
  (reduce merge-resource state srcs))

;;; COMPILE STEPS

(defn find-resources-in-classpath
  "finds all cljs resources in .jar files in the classpath
   ignores PATHS in classpath, add manually if you expect to find cljs there"
  [state]
  (with-logged-time
    [(:logger state) "Find cljs resources in jars"]
    (->> (classpath-entries)
         (filter is-jar?)
         (do-find-resources-in-paths state)
         (merge-resources state)
         )))

(defn find-resources
  "finds cljs resources in the given path"
  ([state path]
   (find-resources state path {:reloadable true}))
  ([state path opts]
   (with-logged-time
     [(:logger state) (format "Find cljs resources in path: \"%s\"" path)]
     (let [file (io/file path)]
       (if (or (not (.exists file))
             (not (.isDirectory file)))
         (throw (ex-info (format "\"%s\" does not exist or is not a directory" path) {:path path}))
         (-> state
             (assoc-in [:source-paths path] (assoc opts
                                              :file file
                                              :abs-path (.getAbsolutePath file)
                                              :path path))
             (merge-resources (do-find-resources-in-paths state [path]))
             ))))))

;; deprecate the weird naming stuff
(def step-find-resources-in-jars find-resources-in-classpath)
(def step-find-resources find-resources)

(def cljs-core-name "cljs/core.cljs")
(def goog-base-name "goog/base.js")

(def ^:dynamic *in-compiler-env* false)

(defmacro with-compiler-env
  "compiler env is a rather big piece of dynamic state
   so we take it out when needed and put the updated version back when done
   doesn't carry the atom arround cause the compiler state itself should be persistent
   thus it should provide safe points

   the body should yield the updated compiler state and not touch the compiler env

   I don't touch the compiler env itself yet at all, might do for some metadata later"
  [state & body]
  `(do (when *in-compiler-env*
         (throw (ex-info "already in compiler env" {})))
       (let [dyn-env# (atom (:compiler-env ~state))
             new-state# (binding [env/*compiler* dyn-env#
                                  *in-compiler-env* true]
                          ~@body)]
         (assoc new-state# :compiler-env @dyn-env#))))

(defn swap-compiler-env!
  [state update-fn & args]
  (if *in-compiler-env*
    (do (swap! env/*compiler* (fn [current] (apply update-fn current args)))
        state)
    (update state :compiler-env (fn [current] (apply update-fn current args)))))

(defn ^:deprecated step-compile-core [state]
  ;; honestly not sure why this was ever here
  ;; since we compile in dep order 'cljs.core will always be compiled before any other CLJS
  state)


(defn discover-macros [{:keys [logger] :as state}]
  ;; build {macro-ns #{used-by-source-by-name ...}}
  (let [macro-info (->> (:sources state)
                        (vals)
                        (filter #(seq (:macros %)))
                        (reduce (fn [macro-info {:keys [macros name]}]
                                  (reduce (fn [macro-info macro-ns]
                                            (update-in macro-info [macro-ns] set-conj name))
                                    macro-info
                                    macros))
                          {})
                        (map (fn [[macro-ns used-by]]
                               (let [name (str (ns->path macro-ns) ".clj")
                                     url (io/resource name)
                                     ;; FIXME: clean this up, must look for .clj and .cljc
                                     [name url] (if url
                                                  [name url]
                                                  (let [name (str name "c")]
                                                    [name (io/resource name)]))]
                                 #_(when-not url (log-warning logger (format "Macro namespace: %s not found, required by %s" macro-ns used-by)))
                                 {:ns macro-ns
                                  :used-by used-by
                                  :name name
                                  :url url})))
                        (map (fn [{:keys [url] :as info}]
                               (if (nil? url)
                                 info
                                 (if (not= "file" (.getProtocol url))
                                   info
                                   (let [file (io/file (.getPath url))]
                                     (assoc info
                                       :file file
                                       :last-modified (.lastModified file)))))))
                        (map (juxt :name identity))
                        (into {}))]
    (assoc state :macros macro-info)
    ))



(defn finalize-config [state]
  (-> state
      (discover-macros)
      (assoc :configured true
             :main-deps {}
             :unoptimizable (when-let [imul (io/resource "cljs/imul.js")]
                              (slurp imul))
             ;; populate index with known sources
             :provide->source (into {} (for [{:keys [name provides]} (vals (:sources state))
                                             provide provides]
                                         [provide name]
                                         )))))

(def step-finalize-config finalize-config)

(defn resolve-main-deps [{:keys [logger] :as state} main-cljs]
  ;; (log-progress logger (format "Resolving deps for: %s" main-cljs))
  (let [deps (get-deps-for-ns state main-cljs)]
    (assoc-in state [:main-deps main-cljs] deps)))

(defn reset-modules [state]
  (-> state
      (assoc :modules {})
      (dissoc :default-module :main-deps :build-modules)
      ))

(defn configure-module
  ([state module-name module-mains depends-on]
   (configure-module state module-name module-mains depends-on {}))
  ([state module-name module-mains depends-on mod-attrs]
   (when-not (keyword? module-name)
     (throw (ex-info "module name should be a keyword" {:module-name module-name})))
   (when-not (every? keyword? depends-on)
     (throw (ex-info "module deps should be keywords" {:module-deps depends-on})))

   (let [is-default? (not (seq depends-on))

         _ (when is-default?
             (when-let [default (:default-module state)]
               (throw (ex-info "default module already defined, are you missing deps?"
                        {:default default :wants-to-be-default module-name}))))

         mod (merge mod-attrs
               {:name module-name
                :js-name (str (name module-name) ".js")
                :mains module-mains
                :depends-on depends-on
                :default is-default?})

         state (assoc-in state [:modules module-name] mod)
         state (if is-default?
                 (assoc state :default-module module-name)
                 state)]
     state)))

(def step-configure-module configure-module)

(defn flush-to-disk
  "flush all generated sources to disk, not terribly useful, use flush-unoptimized to include source maps"
  [{:keys [work-dir sources] :as state}]
  (with-logged-time
    [(:logger state) (format "Flushing to disk")]
    (doseq [{:keys [type name compiled] :as src} (vals sources)
            :when (and (= :cljs type)
                    compiled)]

      (let [{:keys [js-name output]} src
            target (io/file work-dir js-name)]
        (io/make-parents target)
        (spit target output)))
    state))

(defn generate-constants-table [state]
  (if (not (:emit-constants state))
    state
    (let [constants (with-out-str
                      (comp/emit-constants-table
                        (::ana/constant-table (:compiler-env state))))]
      (update-in state [:sources "constants_table.js"] merge {:output constants
                                                              :compiled-at (System/currentTimeMillis)})
      )))


;; module related stuff

(defn module-graph [modules]
  (let [module-set (set (keys modules))]
    (doseq [{:keys [name depends-on]} (vals modules)
            :when (seq depends-on)]
      (when-not (set/subset? depends-on module-set)
        (throw (ex-info "module contains dependency on unknown modules"
                 {:module name
                  :unknown (set/difference depends-on module-set)})))))

  (apply lg/digraph
    (for [{:keys [name depends-on]} (vals modules)
          dep depends-on]
      [name dep])))

(defn closure-defines-and-base [{:keys [public-path] :as state}]
  (let [goog-rc (get-in state [:sources goog-base-name])
        goog-base @(:input goog-rc)]

    (when-not (seq goog-base)
      (throw (ex-info "no goog/base.js" {})))

    ;; FIXME: work arround for older cljs versions that used broked closure release, remove.
    (when (< (count goog-base) 500)
      (throw (ex-info "probably not the goog/base.js you were expecting"
               (get-in state [:sources goog-base-name]))))

    (str "var CLOSURE_NO_DEPS = true;\n"
      ;; goog.findBasePath_() requires a base.js which we dont have
      ;; this is usually only needed for unoptimized builds anyways
      "var CLOSURE_BASE_PATH = '" public-path "/src/';\n"
      "var CLOSURE_DEFINES = "
      (json/write-str (:closure-defines state {}))
      ";\n"
      goog-base
      "\n")))

(defn make-foreign-js-source
  "only need this because we can't control which goog.require gets emitted"
  [{:keys [provides requires]}]
  (let [sb (StringBuilder.)]
    (doseq [provide provides]
      (doto sb
        (.append "goog.provide(\"")
        (.append (munge-goog-ns provide))
        (.append "\");\n")))
    (doseq [require requires]
      (doto sb
        (.append "goog.require(\"")
        (.append (munge-goog-ns require))
        (.append "\");\n")))
    (.toString sb)
    ))

(defn dump-js-modules [modules]
  (doseq [js-mod modules]
    (prn [:js-mod (.getThisAndAllDependencies js-mod)])
    (doseq [input (.getInputs js-mod)]
      (prn [:js-mod input]))))

(defn sort-and-compact-modules
  "sorts modules in dependency order and remove sources provided by parent deps"
  [{:keys [logger modules] :as state}]
  (when-not (seq modules)
    (throw (ex-info "no modules defined" {})))

  ;; if only one module is defined we dont need all this work
  (if (= 1 (count modules))
    (vals modules)
    ;; else: multiple modules must be sorted in dependency order
    (let [module-graph (module-graph modules)
          module-order (reverse (la/topsort module-graph))

          js-mods (reduce
                    (fn [js-mods module-key]
                      (let [{:keys [js-name name depends-on sources]} (get modules module-key)
                            js-mod (JSModule. js-name)]

                        (doseq [name sources]
                          ;; we don't actually need code yet
                          (.add js-mod (SourceFile. name)))

                        (doseq [other-mod-name depends-on
                                :let [other-mod (get js-mods other-mod-name)]]
                          (when-not other-mod
                            (throw (ex-info "module depends on undefined module" {:mod name :other other-mod-name})))
                          (.addDependency js-mod other-mod))

                        (assoc js-mods module-key js-mod)))
                    {}
                    module-order)]

      ;; eek mutable code
      ;; this will move duplicate files from each module to the closest common ancestor
      (doto (JSModuleGraph. (into-array (for [module-key module-order]
                                          (get js-mods module-key))))
        (.coalesceDuplicateFiles))

      (->> module-order
           (map (fn [module-key]
                  (let [module (get modules module-key)
                        ;; enough with the mutable stuff
                        sources (->> (get js-mods module-key)
                                     (.getInputs)
                                     (map #(.getName %))
                                     (vec))]
                    (assoc module :sources sources))))
           (vec)))))

(defn do-print-warnings
  "print warnings after building modules, repeat warnings for files that weren't recompiled!"
  [{:keys [logger] :as state}]
  (doseq [{:keys [name warnings] :as src}
          (->> (:sources state)
               vals
               (sort-by :name)
               (filter #(seq (:warnings %))))]
    (log-warning logger (format "WARNINGS: %s (%d)" name (count warnings)))
    (doseq [warning warnings]
      (log-warning logger warning)
      ))
  state)


(defn get-deps-for-mains [state mains]
  (let [state (reduce resolve-main-deps state mains)]
    (->> mains
         (mapcat #(get-in state [:main-deps %]))
         (distinct)
         (into []))))

(defn do-analyze-module
  "resolve all deps for a given module, based on specified :mains
   will update state for each module with :sources, a list of sources needed to compile this module "
  [state {:keys [name mains] :as module}]
  (assoc-in state [:modules name :sources] (get-deps-for-mains state mains)))

(defn add-foreign
  [state name provides requires js-source externs-source]
  {:pre [(string? name)
         (set? provides)
         (seq provides)
         (set? requires)
         (string? js-source)
         (seq js-source)
         (string? externs-source)
         (seq externs-source)]}

  (merge-resource state {:type :js
                         :foreign true
                         :name name
                         :js-name name
                         :provides provides
                         :requires requires
                         :output js-source
                         :input (atom js-source)
                         :externs-source externs-source
                         }))

(defn make-runtime-setup [{:keys [runtime] :as state}]
  (let [src (str/join "\n" [(case (:print-fn runtime)
                              ;; Browser
                              :console "cljs.core.enable_console_print_BANG_();"
                              ;; Node.JS
                              :print "cljs.core._STAR_print_fn_STAR_ = require(\"util\").print;")])]
    {:type :js
     :name "runtime_setup.js"
     :js-name "runtime_setup.js"
     :provides #{'runtime-setup}
     :requires #{'cljs.core}
     :input (atom src)}))

(defn generate-output-for-source [state {:keys [name type] :as src}]
  (if (seq (:output src))
    src
    (case type
      :js
      (assoc src :output @(:input src))
      :cljs
      (maybe-compile-cljs state src))))


(defn compile-sources
  "compile a list of sources by name,
   requires that the names are in dependency order
   requires that ALL of the dependencies NOT in source names are already compiled
   eg. you cannot just compile [\"clojure/string.cljs\"] as it requires other files to be compiled first"
  [state source-names]
  (with-compiler-env state
    (ana/load-core)
    (reduce
      (fn [state source-name]
        (let [src (get-in state [:sources source-name])
              src (generate-output-for-source state src)]
          (assoc-in state [:sources source-name] src)
          ))
      state
      source-names)))

(defn compile-modules [state]
  (with-logged-time
    [(:logger state) "Compiling Modules ..."]
    (let [state (merge-resource state (make-runtime-setup state))
          state (reduce do-analyze-module state (-> state :modules (vals)))
          modules (sort-and-compact-modules state)
          source-names (mapcat :sources modules)
          state (compile-sources state source-names)]

      (-> state
          (assoc :build-modules modules)
          (generate-constants-table)
          (do-print-warnings)
          ))))

(def step-compile-modules compile-modules)

(defn cljs-source-map-for-module [sm-text sources opts]
  (let [sm-json (json/read-str sm-text :key-fn keyword)
        closure-source-map (sm/decode sm-json)]
    (loop [sources (seq sources)
           merged (sorted-map-by (sm/source-compare (map :name sources)))]
      (if sources
        (let [{:keys [name js-name] :as source} (first sources)]
          (recur
            (next sources)
            (assoc merged name (sm/merge-source-maps (:source-map source)
                                 (get closure-source-map js-name)))))

        ;; FIXME: sm/encode relativizes paths but we already have relative paths
        ;; there is a bunch of work done over there that we simply dont need
        ;; unfortunatly there is no "simple" workarround
        ;; if that ever changes return result of sm/encode instead of reparsing it
        (-> (sm/encode merged
              {:lines (+ (:lineCount sm-json) 2)
               :file (:file sm-json)})
            (json/read-str)
            (assoc "sources" (keys merged))
            ;; sm/encode pprints
            (json/write-str))))))


(defn make-closure-modules
  "make a list of modules (already in dependency order) and create the closure JSModules"
  [state modules]

  (let [js-mods
        (reduce
          (fn [js-mods {:keys [js-name name depends-on sources prepend-js append-js] :as mod}]
            (let [js-mod (JSModule. js-name)]
              (when (:default mod)
                (.add js-mod (SourceFile/fromCode "closure_setup.js" (closure-defines-and-base state))))
              (when (seq prepend-js)
                (.add js-mod (SourceFile/fromCode (str "mod_" name "_prepend.js") prepend-js)))

              (doseq [{:keys [name js-name output] :as src} (map #(get-in state [:sources %]) sources)]
                ;; throws hard to track NPE otherwise
                (when-not (and js-name output (seq output))
                  (throw (ex-info "missing output for source" {:js-name js-name :name (:name src)})))

                (if (:foreign src)
                  (.add js-mod (SourceFile/fromCode js-name (make-foreign-js-source src)))
                  (.add js-mod (SourceFile/fromCode js-name output))))

              (when (seq append-js)
                (.add js-mod (SourceFile/fromCode (str "mod_" name "_append.js") append-js)))

              (doseq [other-mod-name depends-on
                      :let [other-mod (get js-mods other-mod-name)]]
                (when-not other-mod
                  (throw (ex-info "module depends on undefined module" {:mod name :other other-mod-name})))
                (.addDependency js-mod other-mod))

              (assoc js-mods name js-mod)))
          {}
          modules)]
    (for [{:keys [name] :as mod} modules]
      (assoc mod :js-module (get js-mods name))
      )))

(defn- flush-source-maps [{modules :optimized :keys [^File public-dir public-path] :as state}]
  (with-logged-time
    [(:logger state) "Flushing source maps"]

    (when-not (seq modules)
      (throw (ex-info "flush before optimize?" {})))

    (when-not public-dir
      (throw (ex-info "missing :public-dir" {})))

    (doseq [{:keys [source-map-name source-map-json sources] :as mod} modules]
      (let [target (io/file public-dir "src" source-map-name)]
        (io/make-parents target)
        (spit target source-map-json))

      ;; flush all sources used by this module
      ;; FIXME: flushes all files always, should skip if files already exist and are current
      (doseq [{:keys [type name input] :as src} (map #(get-in state [:sources %]) sources)]
        (let [target (io/file public-dir "src" name)]
          (io/make-parents target)
          (spit target @input))))
    state))

;; FIXME: manifest should be custom step
(defn flush-manifest [public-dir modules]
  (spit (io/file public-dir "manifest.json")
    (json/write-str (map #(select-keys % [:name :js-name :mains :depends-on :default :sources]) modules))))

(defn foreign-js-source-for-mod [state {:keys [sources] :as mod}]
  (->> sources
       (map #(get-in state [:sources %]))
       (filter :foreign)
       (map :output)
       (str/join "\n")))

(defn flush-modules-to-disk [{modules :optimized :keys [unoptimizable ^File public-dir public-path logger] :as state}]
  (with-logged-time
    [(:logger state) "Flushing modules to disk"]

    (when-not (seq modules)
      (throw (ex-info "flush before optimize?" {})))

    (when-not public-dir
      (throw (ex-info "missing :public-dir" {})))

    (doseq [{:keys [default output prepend source-map-name name js-name] :as mod} modules]
      (let [target (io/file public-dir js-name)
            out (if default
                  (str unoptimizable output)
                  output)
            out (if (:web-worker mod)
                  (let [deps (:depends-on mod)]
                    (str (str/join "\n" (for [other modules
                                              :when (contains? deps (:name other))]
                                          (str "importScripts('" (:js-name other) "');")))
                      "\n\n"
                      out))
                  out)
            out (str prepend (foreign-js-source-for-mod state mod) out)]

        (io/make-parents target)
        (spit target out)

        (log-progress logger (format "Wrote module \"%s\" (size: %d)" js-name (count out)))

        (when source-map-name
          (spit target (str "\n//# sourceMappingURL=src/" (file-basename source-map-name) "\n")
            :append true)))))

  (flush-manifest public-dir modules)

  (when (:source-map state)
    (flush-source-maps state))

  state)

(defn load-externs [{:keys [build-modules] :as state}]
  (->> build-modules
       (mapcat :sources)
       (map #(get-in state [:sources %]))
       (filter :foreign)
       (reduce (fn [externs {:keys [js-name externs-source] :as foreign-src}]
                 (conj externs (SourceFile/fromCode (str "externs/" js-name) externs-source)))
         (closure/load-externs state))))

(defn closure-optimize
  "takes the current defined modules and runs it through the closure optimizer

   will return the state with :optimized a list of module which now have a js-source and optionally source maps
   nothing is written to disk, use flush-optimized to write"
  [{:keys [logger build-modules] :as state}]
  (when-not (seq build-modules)
    (throw (ex-info "optimize before compile?" {})))

  (with-logged-time
    [logger "Closure optimize"]

    (let [modules (make-closure-modules state build-modules)
          ;; can't use the shared one, that only allows one compile
          cc (make-closure-compiler)
          co (closure/make-options state)

          source-map? (boolean (:source-map state))

          externs (load-externs state)

          result (.compileModules cc externs (map :js-module modules) co)]

      (let [errors (.errors result)
            warnings (.warnings result)]
        (doseq [next (seq errors)]
          (log-warning logger (format "CLOSURE-ERROR: %s" (.toString next))))
        (doseq [next (seq warnings)]
          (log-warning logger (format "CLOSURE-WARNING: %s" (.toString next)))))

      (assoc state
        :optimized (when (.success result)
                     (let [source-map (when source-map?
                                        (.getSourceMap cc))]

                       (vec (for [{:keys [js-name js-module sources] :as m} modules
                                  ;; reset has to be called before .toSource
                                  :let [_ (when source-map?
                                            (.reset source-map))
                                        output (.toSource cc js-module)]]
                              (-> m
                                  (dissoc :js-module)
                                  (merge {:output output}
                                    (when source-map?
                                      (let [sw (StringWriter.)
                                            sources (map #(get-in state [:sources %]) sources)
                                            source-map-name (str js-name ".map")]
                                        (.appendTo source-map sw source-map-name)
                                        {:source-map-json (cljs-source-map-for-module (.toString sw) sources state)
                                         :source-map-name source-map-name})
                                      )))))))))))


(defn- ns-list-string [coll]
  (->> coll
       (map #(str "'" (comp/munge %) "'"))
       (str/join ",")))

(defn closure-goog-deps [state]
  (->> (:sources state)
       (vals)
       (map (fn [{:keys [js-name requires provides]}]
              (str "goog.addDependency(\"" js-name "\","
                "[" (ns-list-string provides) "],"
                "[" (ns-list-string requires) "]);")))
       (str/join "\n")))

(defn flush-sources-by-name
  [{:keys [public-dir] :as state} source-names]
  (doseq [{:keys [type name input last-modified] :as src}
          (->> source-names
               (map #(get-in state [:sources %])))
          :let [target (io/file public-dir "src" name)]

          ;; skip files we already have since source maps are kinda expensive to generate
          :when (or (not (.exists target))
                  (nil? last-modified) ;; runtime-setup doesn't have last-modified
                  (> (or (:compiled-at src) ;; js is not compiled but maybe modified
                       last-modified)
                    (.lastModified target)))]

    (io/make-parents target)

    (case type
      ;; cljs needs to flush the generated .js, for source-maps also the .cljs and a .map
      :cljs
      (do (let [{:keys [source-map js-name output]} src
                js-target (io/file public-dir "src" js-name)]

            (when (nil? output)
              (throw (ex-info (format "no output for resource: %s" js-name) src)))

            (spit js-target output)

            (when source-map
              (let [source-map-name (str js-name ".map")]
                (spit (io/file public-dir "src" source-map-name)
                  (sm/encode {name source-map} {}))
                (spit js-target (str "//# sourceMappingURL=" (file-basename source-map-name) "?r=" (rand)) :append true))))

          ;; spit original source, cljs needed for source maps
          (spit target @input))

      ;; js just needs itself
      ;; FIXME: needs to flush more when js processing is added
      :js
      (spit target (:output src))

      (throw (ex-info "cannot flush" src))
      ))

  state)

(defn flush-unoptimized
  [{:keys [build-modules public-dir public-path unoptimizable] :as state}]
  (when-not (seq build-modules)
    (throw (ex-info "flush before compile?" {})))
  (with-logged-time
    [(:logger state) "Flushing sources"]

    (flush-sources-by-name state (mapcat :sources build-modules)))

  (with-logged-time
    [(:logger state) "Flushing unoptimized modules"]

    (flush-manifest public-dir build-modules)

    ;; flush fake modules
    (doseq [{:keys [default js-name name prepend prepend-js append-js sources web-worker] :as mod} build-modules]
      (let [provided-ns (mapcat #(reverse (get-in state [:sources % :provides]))
                          sources)
            target (io/file public-dir js-name)

            out (->> provided-ns
                     (map (fn [ns]
                            (str "goog.require('" (comp/munge ns) "');")))
                     (str/join "\n"))
            out (str prepend prepend-js out append-js)
            out (if (or default web-worker)
                  ;; default mod needs closure related setup and goog.addDependency stuff
                  (str unoptimizable
                    (when web-worker
                      "\nvar CLOSURE_IMPORT_SCRIPT = function(src) { importScripts(src); };\n")
                    (closure-defines-and-base state)
                    (closure-goog-deps state)
                    "\n\n"
                    out)
                  ;; else
                  out)]

        (spit target out))))
  ;; return unmodified state
  state)

(defn get-reloadable-source-paths [state]
  (->> state
       :source-paths
       (vals)
       (filter :reloadable)
       (map :path)
       (set)))

(defn reload-source [{:keys [url] :as rc}]
  (assoc rc :input (delay (slurp url))))

(defn reset-resource [{:keys [^File file] :as src} config]
  (-> src
      (dissoc :ns :ns-info :requires :provides :output :compiled :compiled-at)
      (reload-source)
      (as-> src'
        (inspect-resource config src'))
      (cond-> file
        (assoc :last-modified (.lastModified file)))))

(defn scan-for-new-files
  "scans the reloadable paths for new files

   returns a seq of resource maps with a {:scan :new} value"
  [{:keys [sources] :as state}]
  (let [reloadable-paths (get-reloadable-source-paths state)
        known-files (->> sources
                         (vals)
                         (map (fn [{:keys [source-path name]}]
                                [source-path name]))
                         (into #{}))]
    (->> reloadable-paths
         (mapcat #(find-fs-resources state %))
         (remove (fn [{:keys [source-path name]}]
                   (contains? known-files [source-path name])))
         (map #(assoc % :scan :new))
         (into []))))

(defn scan-for-modified-files
  "scans known sources for modified or deleted files

  returns a seq of resource maps with a :scan key which is either :modified :delete"
  [{:keys [sources macros] :as state}]
  (let [reloadable-paths (get-reloadable-source-paths state)]

    ;; editing a macro namespace will invalidate cljs files that depend on it
    (let [modified-macros (->> macros
                               (vals)
                               (filter :file)
                               (reduce (fn [result {:keys [used-by file last-modified] :as macro}]
                                         (let [new-mod (.lastModified file)]
                                           (if (<= new-mod last-modified)
                                             result
                                             (let [macro (assoc macro
                                                           :scan :macro
                                                           :last-modified new-mod)]
                                               (->> used-by
                                                    (map #(get-in state [:sources %]))
                                                    (map (fn [rc]
                                                           (assoc rc
                                                             :scan :modified
                                                             :last-modified new-mod)))
                                                    (into (conj result macro)))
                                               ))))
                                 []))]

      (->> (vals sources)
           (filter :file)
           (filter #(contains? reloadable-paths (:source-path %)))
           (reduce (fn [result {:keys [^File file last-modified] :as rc}]
                     (cond
                       (not (.exists file))
                       (conj result (assoc rc :scan :delete))

                       (> (.lastModified file) last-modified)
                       (conj result (assoc rc :scan :modified))

                       :else
                       result))
             modified-macros)))))

(defn scan-files
  "scans for new and modified files
   returns resources maps with a :scan key with is either :new :modified :delete"
  [state]
  (concat (scan-for-modified-files state)
    (scan-for-new-files state)))

(defn wait-for-modified-files!
  "blocks current thread waiting for modified files
  return resource maps with a :scan key which is either :new :modified :delete"
  [{:keys [logger sources] :as initial-state}]
  (log-progress logger "Waiting for modified files ...")
  (loop [state initial-state
         i 0]

    ;; don't scan for new files too frequently
    ;; quite a bit more expensive than just checking a known file

    (let [modified (scan-for-modified-files state)
          modified (if (zero? (mod i 5))
                     (concat modified (scan-for-new-files state))
                     modified)]
      (if (seq modified)
        modified
        (do (Thread/sleep 500)
            (recur state
              (inc i)))))))

(defn reload-modified-files!
  [{:keys [logger] :as state} scan-results]
  (as-> state $state
    (reduce (fn [state {:keys [scan name file] :as rc}]
              (case scan
                :macro
                (do (log-progress logger (format "Macro File modified: %s" name))
                    (require (:ns rc) :reload-all)
                    (assoc-in state [:macros name] (dissoc rc :scan)))
                :delete
                (do (log-progress logger (format "Found deleted file: %s -> %s" name file))
                    (unmerge-resource state (:name rc)))
                :new
                (do (log-progress logger (format "Found new file: %s -> %s" name file))
                    (merge-resources state [(-> rc (dissoc :scan) (reset-resource state))]))

                :modified
                (do (log-progress logger (format "File modified: %s -> %s" name file))
                    (merge-resources state [(-> rc (dissoc :scan) (reset-resource state))]))))
      $state
      scan-results)

    ;; FIXME: this is kinda ugly but need a way to discover newly required macros
    (discover-macros $state)
    ))

(defn wait-and-reload!
  "wait for modified files, reload them and return reloaded state"
  [state]
  (->> (wait-for-modified-files! state)
       (reload-modified-files! state)))

;; configuration stuff
(defn enable-emit-constants [state]
  (-> state
      ;; I can't figure out which setting is actually needed
      ;; seems to mostly be [:options :emit-constants]
      (assoc :emit-constants true
             :optimize-constants true)
      (assoc-in [:compiler-env :opts :emit-constants] true)
      (assoc-in [:compiler-env :opts :optimize-constants] true)
      (assoc-in [:compiler-env :options :emit-constants] true)
      (assoc-in [:compiler-env :options :optimize-constants] true)
      (merge-resource {:type :js
                       :generated true
                       :js-name "constants_table.js"
                       :name "constants_table.js"
                       :provides #{'constants-table}
                       :requires #{}
                       :input (atom "")})))

(defn enable-source-maps [state]
  (assoc state :source-map "cljs.closure/make-options expects a string but we dont use it"))

(defn set-build-options [state opts]
  (merge state opts))

(defn init-state []
  ;; static fns dont work in repl env, but i never used a cljs repl
  ;; need to look into cljs repl, otherwise I see no downside to using static fns
  (alter-var-root #'ana/*cljs-static-fns* (fn [_] true))

  ;; load cljs.core macros, we are probably going to use them
  ;; FIXME: can no longer do this here, intern-macros requires compiler env
  ;; (ana/load-core)

  {:compiler-env {} ;; will become env/*compiler*

   :ignore-patterns #{#"^node_modules/"
                      #"^goog/demos/"
                      #".aot.js$"
                      #"_test.js$"}

   ::is-compiler-state true
   ::cc (make-closure-compiler)

   :runtime {:print-fn :console}
   :macros-loaded #{}
   :use-file-min true

   :manifest-cache-dir (let [dir (io/file "target" "shadow-build" "jar-manifest")]
                         (io/make-parents dir)
                         dir)
   :cache-dir (io/file ".cljs-cache")
   :cache-level :jars

   :source-paths {}
   :closure-defines {"goog.DEBUG" false
                     "goog.LOCALE" "en"}
   :logger (reify BuildLog
             (log-warning [_ msg]
               (println (str "WARN: " msg)))
             (log-time-start [_ msg]
               (println (format "-> %s" msg)))
             (log-time-end [_ msg ms]
               (println (format "<- %s (%dms)" msg ms)))
             (log-progress [_ msg]
               (println msg)))
   })

(defn watch-and-repeat! [state callback]
  (loop [state (callback state [])]
    (let [modified (wait-for-modified-files! state)
          state (reload-modified-files! state modified)]
      (recur (try
               (callback state (mapv :name modified))
               (catch Exception e
                 (println (str "COMPILATION FAILED: " e))
                 (.printStackTrace e)
                 state))))))

(defn has-tests? [{:keys [requires] :as rc}]
  (contains? requires 'cljs.test))

(defn find-dependent-resources [{:keys [provide->source] :as state} source-names]
  (let [graph (apply lg/digraph (for [{:keys [name requires]} (vals (:sources state))
                                      require requires]
                                  [(get provide->source require) name]))]
    (reduce (fn [deps source-name]
              (into deps (la/pre-traverse graph source-name)))
      #{}
      source-names)))
