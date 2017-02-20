(ns shadow.cljs.build
  (:import [java.io File StringWriter FileOutputStream FileInputStream StringReader PushbackReader]
           [java.net URL]
           [com.google.javascript.jscomp JSModule SourceFile SourceFile$Generated SourceFile$Generator SourceFile$Builder JSModuleGraph CustomPassExecutionTime]
           (clojure.lang ExceptionInfo ILookup)
           (java.util.jar JarFile JarEntry)
           (com.google.javascript.jscomp.deps JsFileParser)
           (java.util.logging Level)
           [java.util.concurrent Executors Future]
           (com.google.javascript.jscomp ReplaceCLJSConstants CompilerOptions CommandLineRunner)
           (java.security MessageDigest)
           (javax.xml.bind DatatypeConverter))
  (:require [clojure.data.json :as json]
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
            [clojure.repl :refer (pst)]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [loom.graph :as lg]
            [loom.alg :as la]
            [cognitect.transit :as transit]
            [shadow.cljs.util :as util]
            [shadow.cljs.log :as log]
            [clojure.pprint :refer (pprint)]
            ))

;; (set! *warn-on-reflection* true)

(defn ^com.google.javascript.jscomp.Compiler make-closure-compiler []
  (com.google.javascript.jscomp.Compiler/setLoggingLevel Level/WARNING)
  (doto (com.google.javascript.jscomp.Compiler.)
    ;; the thread lingers and prevents the JVM from exiting
    ;; haven't found a clean way to shut it down otherwise
    ;; but given that only one thread is used to compile anyways there
    ;; is really no gain to running in another thread?
    (.disableThreads)))


(defn write-cache [^File file data]
  (with-open [out (FileOutputStream. file)]
    (let [w (transit/writer out :json {:handlers {URL (transit/write-handler "url" str)}})]
      (transit/write w data)
      )))

(defn read-cache [^File file]
  {:pre [(.exists file)]}
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
      :from-jar boolean
      :last-modified long
      :requires #{name}
      :provides #{name}
      ;; only :cljs
      :ns name
      :ns-info ns-info
      }))

(defn compiler-state? [state]
  (true? (::is-compiler-state state)))

(defn log [state log-event]
  {:pre [(compiler-state? state)]}
  (log/log* (:logger state) state log-event)
  state)

(def ^{:dynamic true} *time-depth* 0)

(defmacro with-logged-time
  [[state msg] & body]
  `(let [msg# ~msg
         start# (System/currentTimeMillis)

         evt#
         (assoc msg#
           :timing :enter
           :start start#
           :depth *time-depth*)]
     (log ~state evt#)
     (let [result#
           (binding [*time-depth* (inc *time-depth*)]
             ~@body)

           stop#
           (System/currentTimeMillis)

           evt#
           (assoc msg#
             :timing :exit
             :depth *time-depth*
             :stop stop#
             :duration (- stop# start#))]
       (log (if (compiler-state? result#) result# ~state) evt#)
       result#)
     ))

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

(defn is-cljc? [^String name]
  (.endsWith name ".cljc"))

(defn is-cljs? [^String name]
  (.endsWith name ".cljs"))

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
      (str/replace #"\.clj(s|c)$" "")
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

(defn add-goog-dependencies [state {:keys [name input] :as rc}]
  {:pre [(compiler-state? state)]}
  (if (= "goog/base.js" name)
    (assoc rc
      :requires #{}
      :require-order []
      :provides #{'goog})
    ;; parse any other js
    (let [deps (-> (JsFileParser. (.getErrorManager (::cc state)))
                   (.parseFile name name @input))]
      (assoc rc
        :requires (into #{} (map munge-goog-ns) (.getRequires deps))
        :require-order (into [] (map munge-goog-ns) (.getRequires deps))
        :provides (into #{} (map munge-goog-ns) (.getProvides deps))))))

(defn macros-from-ns-ast [state {:keys [require-macros use-macros]}]
  {:pre [(compiler-state? state)]}
  (into #{} (concat (vals require-macros) (vals use-macros))))

(defn rewrite-ns-aliases
  [{:keys [requires uses require-order] :as ast}
   {:keys [ns-alias-fn] :as state}]
  (let [should-rewrite
        (->> require-order
             (filter #(ns-alias-fn state %))
             (into #{}))]

    (if-not (seq should-rewrite)
      ast
      (let [rewrite-ns
            (fn [ns]
              (if (contains? should-rewrite ns)
                (ns-alias-fn state ns)
                ns))

            rewrite-ns-map
            (fn [ns-map alias-self?]
              (reduce-kv
                (fn [ns-map alias ns]
                  (if-not (contains? should-rewrite ns)
                    ns-map
                    (let [target (ns-alias-fn state ns)]
                      (-> ns-map
                          (assoc alias target)
                          (cond->
                            alias-self?
                            (assoc ns target))))))
                ns-map
                ns-map))]

        (assoc ast
          :require-order
          (into [] (map rewrite-ns) require-order)
          :requires
          (rewrite-ns-map requires true)
          :uses
          (rewrite-ns-map uses false))
        ))))

(defn update-rc-from-ns
  [state rc {:keys [name require-order] :as ast}]
  {:pre [(compiler-state? state)]}
  (let [require-order
        (if (= 'cljs.core name)
          require-order
          ;; inject implicit deps
          (into '[cljs.core shadow.runtime-setup] require-order))]
    (assoc rc
      :ns name
      :ns-info (dissoc ast :env)
      :provides #{name}
      :macro-namespaces (macros-from-ns-ast state ast)
      :requires (into #{} require-order)
      :require-order require-order)))

(defn error-report
  ([state e]
   (.flush *out*)
   (.write *err* "====== ERROR ==============\n")
   (pst e)
   (.write *err* "===========================\n")
   (.flush *err*))
  ([state e rc]
   (error-report state e)))

(defn peek-into-cljs-resource
  "looks at the first form in a .cljs file, analyzes it if (ns ...) and returns the updated resource
   with ns-related infos"
  [state {:keys [^String name input] :as rc}]
  {:pre [(compiler-state? state)]}
  (let [eof-sentinel (Object.)
        cljc? (is-cljc? name)
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
            (throw (ex-info "file is empty" {:name name}))
            (let [ast (-> (util/parse-ns peek)
                          (rewrite-ns-aliases state))]
              (-> state
                  (update-rc-from-ns rc ast)
                  (assoc :cljc cljc?)))))
        (catch Exception e
          ;; could not parse NS
          ;; be silent about it until we actually require and attempt to compile the file
          ;; make best estimate guess what the file might provide based on name
          (let [guessed-ns (cljs-file->ns name)]
            (assoc rc
              :ns guessed-ns
              :requires #{'cljs.core}
              :require-order ['cljs.core]
              :provides #{guessed-ns}
              :type :cljs
              )))))))

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

(defn extract-foreign-libs
  [{:keys [foreign-libs externs] :as deps} source-path]
  (let [foreign-libs (cond
                       (nil? foreign-libs)
                       []
                       (vector? foreign-libs)
                       foreign-libs
                       (map? foreign-libs)
                       [foreign-libs]
                       (list? foreign-libs)
                       (into [] foreign-libs)
                       :else
                       (throw (ex-info (format "invalid :foreign-libs in deps.cljs of %s" source-path) {:deps deps})))]
    (if (seq externs)
      ;; FIXME: :externs at top level
      (update-in foreign-libs [0 :externs] #(into (or % []) externs))
      foreign-libs)))

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
        abs-path (.getCanonicalPath file)
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
                        :from-jar true
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
  [{:keys [use-file-min] :as state} manifest source-path]
  {:pre [(compiler-state? state)
         (map? manifest)]}
  (let [deps (get manifest "deps.cljs")]
    (if (nil? deps)
      manifest
      (let [foreign-libs (-> @(:input deps)
                             (edn/read-string)
                             (extract-foreign-libs source-path))]

        (reduce
          (fn [result {:keys [externs provides requires] :as foreign-lib}]
            (if-not (or (contains? foreign-lib :file)
                        (contains? foreign-lib :file-min))
              ;; {:externs ["om/externs.js"]}
              ;; doesn't contain any foreign, only externs.
              ;; this really doesn't make sense, currently on the case because of a buggy externs
              ;; in cljsjs/react (pending https://github.com/cljsjs/packages/pull/287)
              result
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
                           :externs externs
                           :externs-source (->> externs
                                                (map #(get result %))
                                                (map :input)
                                                (map deref)
                                                (str/join "\n")))]
                  (-> result
                      (dissoc-all externs)
                      ;; remove :file or :file-min
                      (dissoc (get foreign-lib lib-other))
                      (assoc lib-name rc))))))
          (dissoc manifest "deps.cljs")
          foreign-libs)))))


(defn find-jar-resources
  [{:keys [manifest-cache-dir cache-level] :as state} path]
  {:pre [(compiler-state? state)]}
  ;; FIXME: assuming a jar with the same name and same last modified is always identical, probably not. should md5 the full path?
  (let [manifest-name
        (let [jar (io/file path)]
          (str (.lastModified jar) "-" (.getName jar) ".manifest"))

        mfile
        (io/file manifest-cache-dir manifest-name)

        jar-file
        (io/file path)

        manifest
        (if (and (.exists mfile)
                 (>= (.lastModified mfile) (.lastModified jar-file)))
          (read-jar-manifest mfile)
          (let [manifest (create-jar-manifest state path)]
            (io/make-parents mfile)
            (write-jar-manifest mfile manifest)
            manifest))]
    (-> (process-deps-cljs state manifest path)
        (vals))))

(defn make-fs-resource [state source-path rc-name ^File rc-file]
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
        root-path (.getCanonicalPath root)
        root-len (inc (count root-path))

        manifest
        (->> (for [^File file (file-seq root)
                   :let [file (.getCanonicalFile file)
                         abs-path (.getCanonicalPath file)]
                   :when (and (is-cljs-resource? abs-path)
                              (not (.isHidden file)))
                   :let [name (-> abs-path
                                  (.substring root-len)
                                  (normalize-resource-name))]
                   :when (not (should-ignore-resource? state name))]

               (make-fs-resource state path name file))
             (map (juxt :name identity))
             (into {}))]

    (-> (process-deps-cljs state manifest path)
        (vals))))

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

(defn- get-deps-for-src* [{:keys [deps-stack] :as state} name]
  {:pre [(compiler-state? state)]}
  (when-not (string? name)
    (throw (ex-info (format "trying to get deps for \"%s\"" (pr-str name)) {})))

  (cond
    ;; don't run in circles
    (some #(= name %) deps-stack)
    (let [path (->> (conj deps-stack name)
                    (drop-while #(not= name %))
                    (str/join " -> "))]
      (throw (ex-info (format "circular dependency: %s" path) {:name name :stack deps-stack})))

    ;; don't revisit
    (contains? (:deps-visited state) name)
    state

    :else
    (let [requires (get-in state [:sources name :require-order])]
      (when-not (and requires (vector? requires))
        (throw (ex-info (format "cannot find required deps for \"%s\"" name) {:name name})))

      (let [state (-> state
                      (conj-in [:deps-visited] name)
                      (conj-in [:deps-stack] name))
            state (->> requires
                       (map (fn [require-sym]
                              (let [src-name (get-in state [:provide->source require-sym])]
                                (when-not src-name
                                  (throw
                                    (ex-info
                                      (format "ns \"%s\" not available, required by %s" require-sym name)
                                      {:ns require-sym
                                       :src name})))
                                src-name
                                )))
                       (into [] (distinct))
                       (reduce get-deps-for-src* state))
            state (update state :deps-stack (fn [stack] (into [] (butlast stack))))]
        (conj-in state [:deps-ordered] name)
        ))))

(defn get-deps-for-src
  "returns names of all required sources for a given resource by name (in dependency order), does include self
   (eg. [\"goog/string/string.js\" \"cljs/core.cljs\" \"my-ns.cljs\"])"
  [state src-name]
  {:pre [(compiler-state? state)
         (string? src-name)]}
  (-> state
      (assoc :deps-stack []
             :deps-ordered []
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


(defn post-analyze-ns [{:keys [name] :as ast} opts]
  (let [ast
        (-> ast
            (util/load-macros)
            (util/infer-macro-require)
            (util/infer-macro-use)
            (util/infer-renames-for-macros))]

    (util/check-uses! ast)
    (util/check-renames! ast)
    (swap! env/*compiler* assoc-in [::ana/namespaces name] (dissoc ast :env :op :form))

    ;; FIXME: is this the correct location to do this?
    ;; FIXME: using alter instead of reset, to avoid completely removing meta
    ;; when thing/ns.clj and thing/ns.cljs both have different meta

    (when-let [the-ns (find-ns name)]
      (.alterMeta ^clojure.lang.Namespace the-ns merge (seq [(meta name)])))

    ast))

(defn post-analyze [{:keys [op] :as ast} opts]
  (case op
    :ns (post-analyze-ns ast opts)
    ast))

(defn hijacked-parse-ns [env form name opts]
  (-> (util/parse-ns form)
      (rewrite-ns-aliases opts)
      (assoc :env env :form form :op :ns)))

(def default-parse ana/parse)

(defn shadow-parse [op env form name opts]
  (condp = op
    ;; the default ana/parse 'ns has way too many side effects we don't need or want
    ;; don't want analyze-deps -> never needed
    ;; don't want require or macro ns -> post-analyze
    ;; don't want check-uses -> doesn't recognize macros
    ;; don't want check-use-macros -> doesnt handle (:require [some-ns :refer (a-macro)])
    ;; don't want swap into compiler env -> post-analyze
    'ns (hijacked-parse-ns env form name opts)
    (default-parse op env form name opts)))

(defn analyze
  ([state compile-state form]
   (analyze state compile-state form :statement))
  ([state {:keys [ns name] :as compile-state} form context]
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
       (-> (ana/empty-env) ;; this is anything but empty! requires *cljs-ns*, env/*compiler*
           (assoc :context context)
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

    (binding [comp/*source-map-data*
              (atom {:source-map (sorted-map)
                     :gen-col 0
                     :gen-line 0})]

      (let [result
            (loop [{:keys [ns ns-info] :as compile-state} (assoc init :js "")]
              (let [form
                    (binding [*ns*
                              (create-ns ns)

                              ana/*cljs-ns*
                              ns

                              ana/*cljs-file*
                              name

                              reader/*data-readers*
                              tags/*cljs-data-readers*

                              reader/*alias-map*
                              (merge reader/*alias-map*
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
  (let [ast
        (analyze state compile-state form)

        ast-js
        (with-out-str
          (comp/emit ast))

        compile-state
        (if (= :ns (:op ast))
          (update-rc-from-ns state compile-state ast)
          compile-state)]

    (update-in compile-state [:js] str ast-js)
    ))


(defn warning-collector [build-env warnings warning-type env extra]
  ;; FIXME: currently there is no way to turn off :infer-externs
  ;; the work is always done and the warning is always generated
  ;; it is just not emitted when *warn-in-infer* is not set

  ;; we collect all warnings always since any warning should prevent caching
  ;; :infer-warnings however are very inaccurate so we filter those unless
  ;; explicitly enabled, mirroring what CLJS does more closely.
  (when (or (not= :infer-warning warning-type)
            (get ana/*cljs-warnings* :infer-warning))

    (let [{:keys [line column]}
          env

          msg
          (ana/error-message warning-type extra)]

      (swap! warnings conj
        {:warning warning-type
         :line line
         :column column
         :msg msg
         :extra extra}
        ))))

(defmacro with-warnings
  "given a body that produces a compilation result, collect all warnings and assoc into :warnings"
  [build-env & body]
  `(let [warnings#
         (atom [])

         result#
         (ana/with-warning-handlers
           [(partial warning-collector ~build-env warnings#)]
           ~@body)]

     (assoc result# :warnings @warnings#)))

(defn compile-cljs-string
  [state cljs-source name cljc?]
  (with-warnings state
    (do-compile-cljs-string
      {:name name :ns 'cljs.user}
      (partial default-compile-cljs state)
      cljs-source
      cljc?)))

(defn compile-cljs-seq
  [state cljs-forms name]
  (with-warnings state
    (reduce
      (partial default-compile-cljs state)
      {:name name :ns 'cljs.user}
      cljs-forms)))

(defn do-compile-cljs-resource
  "given the compiler state and a cljs resource, compile it and return the updated resource
   should not touch global state"
  [{:keys [static-fns elide-asserts] :as state} {:keys [name input] :as rc}]

  (binding [ana/*cljs-static-fns*
            static-fns

            ;; initialize with default value
            ;; must set binding to it is thread bound, since the analyzer may set! it
            ana/*unchecked-if*
            ana/*unchecked-if*

            ;; root binding for warnings so (set! *warn-on-infer* true) can work
            ana/*cljs-warnings*
            ana/*cljs-warnings*

            *assert*
            (not= elide-asserts true)]

    (let [source @input]
      (with-logged-time
        [state {:type :compile-cljs :name name}]

        (let [{:keys [js ns requires require-order source-map warnings]}
              (cond
                (string? source)
                (compile-cljs-string state source name (is-cljc? name))
                (vector? source)
                (compile-cljs-seq state source name)
                :else
                (throw (ex-info "invalid cljs source type" {:name name :source source})))]

          (when-not ns
            (throw (ex-info "cljs file did not provide a namespace" {:file name})))

          (assoc rc
            :output js
            :requires requires
            :require-order require-order
            :compiled-at (System/currentTimeMillis)
            :provides #{ns}
            :compiled true
            :warnings warnings
            :source-map source-map))))))


;; FIXME: must manually bump if anything cache related changes
;; use something similar to clojurescript-version
(def cache-file-version "v8")

(defn get-cache-file-for-rc
  ^File [{:keys [cache-dir] :as state} {:keys [name] :as rc}]
  (io/file cache-dir "ana" (str name "." cache-file-version ".cache.transit.json")))

(defn get-max-last-modified-for-source [state source-name]
  (let [{:keys [last-modified macro-namespaces] :as rc}
        (get-in state [:sources source-name])]

    (transduce
      (map #(get-in state [:macros % :last-modified]))
      (completing
        (fn [a b]
          (Math/max ^long a ^long b)))
      last-modified
      macro-namespaces
      )))

(defn make-age-map
  "procudes a map of {source-name last-modified} for caching to identify
   whether a cache is safe to use (if any last-modifieds to not match if is safer to recompile)"
  [state ns]
  (reduce
    (fn [age-map source-name]
      (let [last-modified (get-max-last-modified-for-source state source-name)]
        ;; zero? is a pretty ugly indicator for deps that should not affect cache
        ;; eg. shadow.runtime-setup
        (if (pos? last-modified)
          (assoc age-map source-name last-modified)
          age-map)))
    {}
    (get-deps-for-ns state ns)))


(def cache-affecting-options
  [:static-fns
   :elide-asserts
   :node-global-prefix
   :source-map])

(defn load-cached-cljs-resource
  [{:keys [cache-dir cljs-runtime-path] :as state}
   {:keys [ns js-name name last-modified] :as rc}]
  (let [cache-file (get-cache-file-for-rc state rc)
        cache-js (io/file cache-dir cljs-runtime-path js-name)]

    (try
      (when (and (.exists cache-file)
                 (> (.lastModified cache-file) last-modified)
                 (.exists cache-js)
                 (> (.lastModified cache-js) last-modified))

        (let [cache-data
              (read-cache cache-file)

              age-of-deps
              (make-age-map state ns)]

          ;; just checking the "maximum" last-modified of all dependencies is not enough
          ;; must check times of all deps, mostly to guard against jar changes
          ;; lib-A v1 was released 3 days ago
          ;; lib-A v2 was released 1 day ago
          ;; we depend on lib-A and compile against v1 today
          ;; realize that a new version exists and update deps
          ;; compile again .. since we were compiled today the min-age is today
          ;; which is larger than v2 release date thereby using cache if only checking one timestamp

          (when (and (= (:source-path cache-data) (:source-path rc))
                     (= age-of-deps (:age-of-deps cache-data))
                     (every? #(= (get state %) (get-in cache-data [:cache-options %])) cache-affecting-options))
            (log state {:type :cache-read
                        :name name})

            ;; restore analysis data
            (let [ana-data (:analyzer cache-data)]

              (swap! env/*compiler* assoc-in [::ana/namespaces (:ns cache-data)] ana-data)
              (util/load-macros ana-data))

            ;; merge resource data & return it
            (-> (merge rc cache-data)
                (dissoc :analyzer :cache-options :age-of-deps)
                (assoc :cached true
                       :output (slurp cache-js))))))

      (catch Exception e
        (log state {:type :cache-error
                    :ns ns
                    :name name
                    :error e})
        nil))))

(defn write-cached-cljs-resource
  [{:keys [ns name js-name] :as rc} {:keys [cache-dir cljs-runtime-path] :as state}]

  ;; only cache files that don't have warnings!
  (when-not (seq (:warnings rc))
    (let [cache-file
          (get-cache-file-for-rc state rc)]

      (try
        (let [cache-data
              (-> rc
                  (dissoc :file :output :input :url)
                  (assoc :age-of-deps (make-age-map state ns)
                         :analyzer (get-in @env/*compiler* [::ana/namespaces ns])))

              cache-options
              (reduce
                (fn [cache-options option-key]
                  (assoc cache-options option-key (get state option-key)))
                {}
                cache-affecting-options)

              cache-data
              (assoc cache-data :cache-options cache-options)

              cache-js
              (io/file cache-dir cljs-runtime-path js-name)]

          (io/make-parents cache-file)
          (write-cache cache-file cache-data)

          (io/make-parents cache-js)
          (spit cache-js (:output rc))

          (log state {:type :cache-write
                      :ns ns
                      :name name})

          true)
        (catch Exception e
          (log state {:type :cache-error
                      :ns ns
                      :name name
                      :error e})
          nil)
        ))))

(defn maybe-compile-cljs
  "take current state and cljs resource to compile
   make sure you are in with-compiler-env"
  [{:keys [cache-dir cache-level] :as state} {:keys [from-jar file] :as src}]
  (let [cache? (and cache-dir
                    ;; even with :all only cache resources that are in jars or have a file
                    ;; cljs.user (from repl) or shadow.runtime-setup should never be cached
                    (or (and (= cache-level :all)
                             (or from-jar file))
                        (and (= cache-level :jars)
                             from-jar)))]
    (or (when cache?
          (load-cached-cljs-resource state src))
        (let [compiled-src
              (do-compile-cljs-resource state src)]

          (when cache?
            (write-cached-cljs-resource compiled-src state))

          ;; fresh compiled, not from cache
          (assoc compiled-src :cached false)))))

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

(defn valid-resource? [{:keys [type input name provides requires require-order last-modified] :as src}]
  (and (contains? #{:js :cljs} type)
       (instance? clojure.lang.IDeref input)
       (string? name)
       (set? provides)
       (set? requires)
       (vector? require-order)
       (number? last-modified)
       ))

(defn merge-resource
  [state {:keys [name provides url] :as src}]
  (cond
    (not (valid-resource? src))
    (do (log state {:type :invalid-resource
                    :name name
                    :url url})
        state)

    (and (= :js (:type src))
         (contains? (:provides src) 'cljs.core))
    (do (log state {:type :bad-resource
                    :reason :compiled-cljs
                    :url url
                    :msg (format "Ignoring bad file, it attempted to provide cljs.core%n%s" url)})
        state)

    ;; no not merge files that don't have the expected path for their ns
    ;; not really needed but cljs does this, so we should enforce it as well
    (and (= :cljs (:type src))
         (symbol? (:ns src))
         (let [expected-name (ns->cljs-file (:ns src))
               expected-cljc (str/replace expected-name #".cljs$" ".cljc")]
           (not (or (= name expected-name)
                    (= name expected-cljc)
                    ))))

    (do (log state
          {:type :name-violation
           :src src
           :expected (str (ns->cljs-file (:ns src)) " (or .cljc)")})
        ;; still want to remember the resource so it doesn't get detected as new all the time
        ;; remove all provides, otherwise it might end up being used despite the invalid name
        ;; enforce this behavior since the warning might get overlooked easily
        (let [invalid-src (assoc src
                            :provides #{}
                            :requires #{}
                            :require-order [])]
          (assoc-in state [:sources name] invalid-src)))

    ;; do not merge files that are already present from a different source path
    (let [existing (get-in state [:sources name])]
      (and existing
           (or (not= (:source-path existing)
                     (:source-path src))
               (not= (:url existing)
                     (:url src)))))
    (do (log state
          {:type :bad-resource
           :reason :duplicate
           :name name
           :path-use (get-in state [:sources name :source-path])
           :path-ignore (:source-path src)
           :msg (format
                  "duplicate file on classpath \"%s\" (using A)%nA: %s%nB: %s"
                  name
                  (get-in state [:sources name :source-path])
                  (:source-path src))})
        state)

    ;; now we need to handle conflicts for cljc/cljs files
    ;; only use cljs if both exist
    :valid-resource
    (let [cljc?
          (is-cljc? name)

          cljc-name
          (when (is-cljs? name)
            (str/replace name #"cljs$" "cljc"))

          cljs-name
          (when cljc?
            (str/replace name #"cljc$" "cljs"))]

      (cond
        ;; don't merge .cljc file if a .cljs of the same name exists
        (and cljc? (contains? (:sources state) cljs-name))
        ;; less noise by not complaining
        (do #_(log state {:type :bad-resource
                          :reason :cljs-over-cljc
                          :name name
                          :cljc-name name
                          :cljs-name cljs-name
                          :msg (format "File conflict: \"%s\" -> \"%s\" (using \"%s\")" name cljs-name cljs-name)})
          state)

        ;; if a .cljc exists for a .cljs file
        ;; overrides provides from .cljc with provides in .cljs
        (and (is-cljs? name) (contains? (:sources state) cljc-name))
        (-> state
            (unmerge-resource cljc-name)
            (assoc-in [:sources name] src)
            (merge-provides name provides))

        :no-conflict
        (-> state
            (assoc-in [:sources name] src)
            (merge-provides name provides))))))

(defn merge-resources [state srcs]
  (reduce merge-resource state srcs))

;;; COMPILE STEPS

(defn do-find-resources-in-path [state path]
  {:pre [(compiler-state? state)]}
  (if (is-jar? path)
    (find-jar-resources state path)
    (find-fs-resources state path)))

(defn should-exclude-classpath [exclude path]
  (boolean (some #(re-find % path) exclude)))

(defn merge-resources-in-path
  ([state path]
   (merge-resources-in-path state path {:reloadable true}))
  ([state path path-opts]
   (let [file (.getCanonicalFile (io/file path))
         abs-path (.getCanonicalPath file)]
     ;; checkout deps with a lot of symlinks can cause duplicates on classpath
     (if (contains? (:source-paths state) abs-path)
       state
       (let [resources (do-find-resources-in-path state abs-path)
             state
             (if (.isDirectory file)
               (assoc-in state [:source-paths abs-path] (assoc path-opts
                                                          :file file
                                                          :path abs-path))
               state)]
         (merge-resources state resources))))))

(defn find-resources
  "finds cljs resources in the given path"
  ([state path]
   (find-resources state path {:reloadable true}))
  ([state path opts]
   (with-logged-time
     [state {:type :find-resources
             :path path}]
     (let [file (io/file path)
           abs-path (.getCanonicalPath file)]
       (when-not (.exists file)
         (throw (ex-info (format "\"%s\" does not exist" path) {:path path})))

       (if (contains? (:source-paths state) abs-path)
         (do (log state {:type :duplicate-path
                         :path path})
             state)
         (merge-resources-in-path state path opts))
       ))))

(defn find-resources-in-classpath
  "finds all cljs resources in the classpath (ignores resources)"
  ([state]
   (find-resources-in-classpath state {:exclude (:classpath-excludes state [])}))
  ([state {:keys [exclude]}]
   (with-logged-time
     [state {:type :find-resources-classpath}]
     (let [paths
           (->> (classpath-entries)
                (remove #(should-exclude-classpath exclude %)))]
       (reduce merge-resources-in-path state paths)
       ))))

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

(defn discover-macros [state]
  ;; build {macro-ns #{used-by-source-by-name ...}}
  (let [macro-info
        (->> (:sources state)
             (vals)
             (filter #(seq (:macro-namespaces %)))
             (reduce (fn [macro-info {:keys [macro-namespaces name]}]
                       (reduce (fn [macro-info macro-ns]
                                 (update-in macro-info [macro-ns] set-conj name))
                         macro-info
                         macro-namespaces))
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
             ;; always get last modified for macro source
             (map (fn [{:keys [url] :as info}]
                    (if (nil? url)
                      info
                      (let [con (.openConnection url)]
                        (assoc info :last-modified (.getLastModified con)))
                      )))
             ;; get file (if not in jar)
             (map (fn [{:keys [^URL url] :as info}]
                    (if (nil? url)
                      info
                      (if (not= "file" (.getProtocol url))
                        info
                        (let [file (io/file (.getPath url))]
                          (assoc info :file file))))))
             (map (juxt :ns identity))
             (into {}))]
    (assoc state :macros macro-info)
    ))

(defn set-default-compiler-env
  [state]
  (cond-> state
    (nil? (:compiler-env state))
    (assoc :compiler-env @(env/default-compiler-env state))))

(defn finalize-config
  "should be called AFTER all resources have been discovered (ie. after find-resources...)"
  [state]
  (-> state
      (discover-macros)
      (set-default-compiler-env)
      (assoc
        :configured
        true

        :unoptimizable
        (when-let [imul (io/resource "cljs/imul.js")]
          (slurp imul))

        ;; populate index with known sources
        :provide->source
        (into {} (for [{:keys [name provides]} (vals (:sources state))
                       provide provides]
                   [provide name]
                   )))))

(defn reset-modules [state]
  (-> state
      (assoc :modules {})
      (dissoc :default-module :build-modules)
      ))

(defn configure-module
  ([state module-name module-entries depends-on]
   (configure-module state module-name module-entries depends-on {}))
  ([state module-name module-entries depends-on mod-attrs]
   (when-not (keyword? module-name)
     (throw (ex-info "module name should be a keyword" {:module-name module-name})))
   (when-not (every? keyword? depends-on)
     (throw (ex-info "module deps should be keywords" {:module-deps depends-on})))

   (let [is-default?
         (not (seq depends-on))

         mod
         (merge mod-attrs
           {:name module-name
            :js-name (str (name module-name) ".js")
            :entries module-entries
            :depends-on (into #{} depends-on)
            :default is-default?})]

     (when is-default?
       (when-let [default (:default-module state)]
         (throw (ex-info "default module already defined, are you missing deps?"
                  {:default default :wants-to-be-default module-name}))))

     (-> state
         (assoc-in [:modules module-name] mod)
         (cond->
           is-default?
           (assoc :default-module module-name)
           )))))

(defn flush-to-disk
  "flush all generated sources to disk, not terribly useful, use flush-unoptimized to include source maps"
  [{:keys [work-dir sources] :as state}]
  (with-logged-time
    [state {:type :flush-to-disk}]
    (doseq [{:keys [type name compiled] :as src} (vals sources)
            :when (and (= :cljs type)
                       compiled)]

      (let [{:keys [js-name output]} src
            target (io/file work-dir js-name)]
        (io/make-parents target)
        (spit target output)))
    state))

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

(defn closure-defines-and-base [{:keys [public-path cljs-runtime-path] :as state}]
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
         "var CLOSURE_BASE_PATH = '" public-path "/" cljs-runtime-path "/';\n"
         "var CLOSURE_DEFINES = "
         (json/write-str (:closure-defines state {}))
         ";\n"
         goog-base
         "\n")))

(defn make-foreign-js-source
  "only need this because we can't control which goog.require gets emitted"
  [{:keys [provides require-order]}]
  (let [sb (StringBuilder.)]
    (doseq [provide provides]
      (doto sb
        (.append "goog.provide(\"")
        (.append (munge-goog-ns provide))
        (.append "\");\n")))
    (doseq [require require-order]
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
  [{:keys [modules] :as state}]
  (when-not (seq modules)
    (throw (ex-info "no modules defined" {})))

  ;; if only one module is defined we dont need all this work
  (if (= 1 (count modules))
    (vec (vals modules))
    ;; else: multiple modules must be sorted in dependency order
    (let [module-graph
          (module-graph modules)

          module-order
          (reverse (la/topsort module-graph))

          js-mods
          (reduce
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

(defn print-warnings!
  "print warnings after building modules, repeat warnings for files that weren't recompiled!"
  [state]
  (doseq [{:keys [name warnings] :as src}
          (->> (:build-sources state)
               (map #(get-in state [:sources %]))
               (sort-by :name)
               (filter #(seq (:warnings %))))]
    (doseq [warning warnings]

      (log state {:type :warning
                  :name name
                  :warning warning})
      )))

(defn get-deps-for-entries [state entries]
  (->> entries
       (mapcat #(get-deps-for-ns state %))
       (distinct)
       (into [])))

(defn do-analyze-module
  "resolve all deps for a given module, based on specified :entries
   will update state for each module with :sources, a list of sources needed to compile this module "
  [state {:keys [name entries] :as module}]
  (assoc-in state [:modules name :sources] (get-deps-for-entries state entries)))

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

  (merge-resource state
    {:type :js
     :foreign true
     :name name
     :js-name name
     :provides provides
     :requires requires
     ;; FIXME: should allow getting a vector as provides instead
     :require-order (into [] requires)
     :output js-source
     :input (atom js-source)
     :externs-source externs-source
     :last-modified 0
     }))

(defn make-runtime-setup
  [{:keys [runtime] :as state}]
  (let [src (str/join "\n"
              ["goog.provide('shadow.runtime_setup');"
               "goog.require('cljs.core');"
               (case (:print-fn runtime)
                 ;; Browser
                 :console "cljs.core.enable_console_print_BANG_();"
                 ;; Node.JS
                 :print "cljs.core._STAR_print_fn_STAR_ = require(\"util\").print;")])]
    {:type :js
     :name "shadow/runtime_setup.js"
     :js-name "shadow/runtime_setup.js"
     :ns 'shadow.runtime-setup
     :provides #{'shadow.runtime-setup}
     :requires #{'cljs.core}
     :require-order ['cljs.core]
     :input (atom src)
     :last-modified 0 ;; this file should never cause recompiles
     }))


(defn generate-output-for-source [state {:keys [name type output warnings] :as src}]
  {:pre [(valid-resource? src)]}
  (if (and (seq output)
           ;; always recompile files with warnings
           (not (seq warnings)))
    src
    (case type
      :js
      (if output
        (assoc src :cached true)
        (assoc src :output @(:input src)
                   :cached false))
      :cljs
      (maybe-compile-cljs state src))))

(defn do-compile-sources
  "compiles with just the main thread, can do partial compiles assuming deps are compiled"
  [state source-names]
  (with-redefs [ana/parse shadow-parse]
    (with-compiler-env state
      (ana/load-core)
      (reduce
        (fn [state source-name]
          (let [src (get-in state [:sources source-name])
                compiled-src (generate-output-for-source state src)]
            (assoc-in state [:sources source-name] compiled-src)))
        state
        source-names))))

(defn par-compile-one
  [state ready-ref errors-ref source-name]
  (let [{:keys [requires] :as src}
        (get-in state [:sources source-name])]

    (loop [idle-count 0]
      (let [ready @ready-ref]
        (cond
          ;; skip work if errors occured
          (seq @errors-ref)
          src

          ;; only compile once all dependencies are compiled
          ;; FIXME: sleep is not great, cljs.core takes a couple of sec to compile
          ;; this will spin a couple hundred times, doing additional work
          ;; don't increase the sleep time since many files compile in the 5-10 range
          (not (set/superset? ready requires))
          (do (Thread/sleep 5)
              ;; diagnostic warning if we are still waiting for something to compile for 15+ sec
              ;; should only happen in case of deadlocks or missing/broken requires
              ;; should probably add a real timeout and fail the build instead of looping forever
              (if (>= idle-count 3000)
                (let [pending (set/difference requires ready)]
                  (log state {:type :pending-compile
                              :source-name source-name
                              :pending pending})
                  (recur 0))
                (recur (inc idle-count))))

          :ready-to-compile
          (try
            (let [expected-provides
                  (:provides src)

                  {:keys [provides] :as compiled-src}
                  (generate-output-for-source state src)]

              (when (not= expected-provides provides)
                (throw (ex-info "generated output did not produce expected provides"
                         {:expected expected-provides
                          :provides provides
                          :source-name source-name})))

              (swap! ready-ref set/union provides)
              compiled-src)
            (catch Exception e
              (swap! errors-ref assoc source-name e)
              src
              )))))))

(defn par-compile-sources
  "compile files in parallel, files MUST be in dependency order and ALL dependencies must be present
   this cannot do a partial incremental compile"
  [{:keys [n-compile-threads] :as state} source-names]
  (with-redefs [ana/parse shadow-parse]
    (with-compiler-env state
      (ana/load-core)
      (let [;; namespaces that are ready to be used
            ready
            (atom #{})

            ;; source-name -> exception
            errors
            (atom {})

            exec
            (Executors/newFixedThreadPool n-compile-threads)

            tasks
            (->> (for [source-name source-names]
                   ;; bound-fn for with-compiler-state
                   (let [task-fn (bound-fn [] (par-compile-one state ready errors source-name))]
                     (.submit exec ^Callable task-fn)))
                 (doall) ;; force submit all, then deref
                 (into [] (map deref)))]

        ;; FIXME: might deadlock here if any of the derefs fail
        (.shutdown exec)

        ;; unlikely to encounter 2 concurrent errors
        ;; so unpack for a single error for better stacktrace
        (let [errs @errors]
          (case (count errs)
            0 nil
            1 (let [[name err] (first errs)]
                (throw (ex-info (format "compilation of \"%s\" failed" name) {} err)))
            (throw (ex-info "compilation failed" errs))))

        (-> state
            (update :sources (fn [sources]
                               (reduce
                                 (fn [sources {:keys [name] :as src}]
                                   (assoc sources name src))
                                 sources
                                 tasks)))
            )))))

(defn names-compiled-in-last-build [{:keys [build-sources build-start] :as state}]
  (->> build-sources
       (filter
         (fn [name]
           (let [{:keys [compiled compiled-at cached] :as rc}
                 (get-in state [:sources name])]
             (and (not cached) compiled (> compiled-at build-start))
             )))
       (into [])))

(defn compile-sources
  [{:keys [n-compile-threads] :as state} source-names]
  "compile a list of sources by name,
   requires that the names are in dependency order
   requires that ALL of the dependencies NOT in source names are already compiled
   eg. you cannot just compile [\"clojure/string.cljs\"] as it requires other files to be compiled first"
  (log state {:type :compile-sources
              :n-compile-threads n-compile-threads
              :source-names source-names})
  (-> state
      (assoc :build-start (System/currentTimeMillis))
      (cond->
        (> n-compile-threads 1)
        (par-compile-sources source-names)

        (not (> n-compile-threads 1))
        (do-compile-sources source-names))
      (assoc :build-done (System/currentTimeMillis))))

(defn do-compile-modules
  "compiles source based on :build-modules (created by prepare-modules)"
  [{:keys [build-sources] :as state}]
  (compile-sources state build-sources))

(defn prepare-compile
  "prepares for compilation (eg. create source lookup index, shadow.runtime-setup)"
  [state]
  ;; FIXME: re-creating shadow.runtime-setup with every incremental compile
  ;; should do this once?
  (let [runtime-setup
        (make-runtime-setup state)]
    (-> (finalize-config state)
        (merge-resource runtime-setup)
        )))

(defn prepare-modules
  "prepares :modules for compilation (sort and compacts duplicate sources)"
  [state]
  (let [state
        (reduce do-analyze-module state (-> state :modules (vals)))

        modules
        (sort-and-compact-modules state)]
    (assoc state
      :build-modules modules
      :build-sources (into [] (mapcat :sources modules)))))

(defn compile-modules
  "compiles according to configured :modules"
  [state]
  (with-logged-time
    [state {:type :compile-modules}]
    (let [state
          (-> state
              (prepare-compile)
              (prepare-modules)
              (do-compile-modules))]

      (print-warnings! state)

      state
      )))

(defn compile-all-for-ns
  "compiles files to given ns, ignores configured :modules"
  [state ns]
  (let [state
        (prepare-compile state)

        deps
        (get-deps-for-ns state ns)]

    (-> state
        (assoc :build-sources deps)
        (compile-sources deps))
    ))

;; FIXME: need to investige this further
;; closure produces a source map, we want to map the names in that back to cljs names
;; but the lines don't match up correctly, leaving us with a basically useless source map
(defn cljs-source-map-for-module
  "this does not work when using modules as the lines are all screwed up"
  [state {module-name :name :as module} source-names sm-text]
  (let [sm-json
        (json/read-str sm-text :key-fn keyword)

        closure-source-map
        (sm/decode-reverse sm-json)

        merged
        (reduce
          (fn [merged source-name]
            (let [{:keys [type name source-map] :as source}
                  (get-in state [:sources source-name])]

              (if (not= :cljs type)
                merged
                (let [closure-map
                      (get closure-source-map name)

                      map
                      (sm/merge-source-maps source-map closure-map)]

                  (assoc merged name map)))))

          (sorted-map-by (sm/source-compare source-names))
          source-names)]

    (let [res
          (-> (sm/encode* merged
                {:lines (+ (:lineCount sm-json) 2)
                 :file (:file sm-json)})
              (assoc "sources" (keys merged)))]
      (json/write-str res))))


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

              (doseq [{:keys [name type js-name output] :as src} (map #(get-in state [:sources %]) sources)]
                ;; throws hard to track NPE otherwise
                (when-not (and js-name output (seq output))
                  (throw (ex-info "missing output for source" {:js-name js-name :name (:name src)})))

                (if (:foreign src)
                  (.add js-mod (SourceFile/fromCode name (make-foreign-js-source src)))
                  (.add js-mod (SourceFile/fromCode name output))))

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

(defn- flush-source-maps [{modules :optimized :keys [^File public-dir cljs-runtime-path] :as state}]
  (with-logged-time
    [state {:type :flush-source-maps}]

    (when-not (seq modules)
      (throw (ex-info "flush before optimize?" {})))

    (when-not public-dir
      (throw (ex-info "missing :public-dir" {})))

    (doseq [{:keys [source-map-name source-map-json sources] :as mod} modules]
      (let [target (io/file public-dir cljs-runtime-path source-map-name)]
        (io/make-parents target)
        (spit target source-map-json))

      ;; flush all sources used by this module
      ;; FIXME: flushes all files always, should skip if files already exist and are current
      (doseq [{:keys [type name input] :as src} (map #(get-in state [:sources %]) sources)]
        (let [target (io/file public-dir cljs-runtime-path name)]
          (io/make-parents target)
          (spit target @input))))
    state))

;; FIXME: manifest should be custom step
(defn flush-manifest [public-dir modules]
  (spit
    (io/file public-dir "manifest.json")
    (->> modules
         (map (fn [{:keys [name js-name entries depends-on default sources foreign-files] :as mod}]
                (-> {:name name
                     :js-name js-name
                     :entries entries
                     :depends-on depends-on
                     :default default
                     :sources sources}
                    (cond->
                      (seq foreign-files)
                      (assoc :foreign (mapv #(select-keys % [:js-name :provides]) foreign-files))))
                ))
         (json/write-str))))

(defn foreign-js-source-for-mod [state {:keys [sources] :as mod}]
  (->> sources
       (map #(get-in state [:sources %]))
       (filter :foreign)
       (map :output)
       (str/join "\n")))

(defn md5hex [^String text]
  (let [bytes
        (.getBytes text)

        md
        (doto (MessageDigest/getInstance "MD5")
          (.update bytes))

        sig
        (.digest md)]

    (DatatypeConverter/printHexBinary sig)
    ))

(defn create-foreign-bundles
  [state]
  (update state :optimized
    (fn [modules]
      (->> modules
           (map (fn [{:keys [name sources] :as mod}]
                  (let [foreign-srcs
                        (->> sources
                             (map #(get-in state [:sources %]))
                             (filter :foreign))]

                    (if-not (seq foreign-srcs)
                      mod
                      (let [output
                            (->> foreign-srcs
                                 (map :output)
                                 (str/join "\n"))

                            provides
                            (->> foreign-srcs
                                 (map :provides)
                                 (reduce set/union #{})
                                 (sort)
                                 (into []))

                            md5
                            (md5hex output)

                            foreign-name
                            (str (clojure.core/name name) "-foreign-" md5 ".js")]

                        (assoc mod
                          ;; remove foreign from sources list (mainly for a clean manifest)
                          :sources
                          (->> sources
                               (map #(get-in state [:sources %]))
                               (remove :foreign)
                               (map :name)
                               (into []))

                          ;; FIXME: unfinished optmization
                          ;; I want to be able to split foreigns into multiple files eventually
                          ;; say you have foreign A,B,C
                          ;; if A changes more frequently than B,C it might be useful to have it as a separate include
                          ;; so B,C still benefit from caching and A doesn't cause them to download again
                          :foreign-files
                          [{:js-name foreign-name
                            :provides provides
                            :output output
                            }])))
                    )))
           (into [])))))

(defn flush-foreign-bundles
  [{:keys [public-dir] :as state}]
  (doseq [{:keys [foreign-files] :as mod} (:optimized state)
          :when (seq foreign-files)
          {:keys [js-name provides output] :as foreign-file} foreign-files]
    (let [target (io/file public-dir js-name)]
      (when-not (.exists target)

        (log state {:type :flush-foreign
                    :provides provides
                    :js-name js-name
                    :js-size (count output)})

        (spit target output))))

  state)

(defn flush-modules-to-disk
  [{modules :optimized
    :keys [unoptimizable
           bundle-foreign
           ^File public-dir
           cljs-runtime-path]
    :as state}]

  (with-logged-time
    [state {:type :flush-optimized}]

    (when-not (seq modules)
      (throw (ex-info "flush before optimize?" {})))

    (when-not public-dir
      (throw (ex-info "missing :public-dir" {})))

    (let [{modules :optimized
           :as state}
          (if (not= :inline bundle-foreign)
            (-> state
                (create-foreign-bundles)
                (flush-foreign-bundles))
            state)]

      (doseq [{:keys [default output prepend source-map-name name js-name] :as mod} modules]
        (let [target
              (io/file public-dir js-name)

              out
              (if default
                (str unoptimizable output)
                output)

              out
              (if (:web-worker mod)
                (let [deps (:depends-on mod)]
                  (str (str/join "\n" (for [other modules
                                            :when (contains? deps (:name other))]
                                        (str "importScripts('" (:js-name other) "');")))
                       "\n\n"
                       out))
                out)

              out
              (if (= :inline bundle-foreign)
                (str prepend (foreign-js-source-for-mod state mod) out)
                (str prepend "\n" out))]

          (io/make-parents target)
          (spit target out)

          (log state {:type :flush-module
                      :name name
                      :js-name js-name
                      :js-size (count out)})

          (when source-map-name
            (spit target (str "\n//# sourceMappingURL=" cljs-runtime-path "/" (file-basename source-map-name) "\n")
              :append true))))

      (flush-manifest public-dir modules)

      (when (:source-map state)
        (flush-source-maps state))

      ;; with-logged-time expects that we return the compiler-state
      state)))

(defn load-externs [{:keys [externs build-modules] :as state}]
  (let [default-externs
        (CommandLineRunner/getDefaultExterns)

        manual-externs
        (when (seq externs)
          (->> externs
               (map
                 (fn [ext]
                   (if-let [rc (or (io/resource ext)
                                   (let [file (io/file ext)]
                                     (when (.exists file)
                                       file)))]
                     (SourceFile/fromCode (str "EXTERNS/" ext) (slurp rc))
                     (do (log state {:type :missing-externs
                                     :extern ext})
                         nil))))
               (remove nil?)
               ;; just to force the logging
               (into [])))

        foreign-externs
        (->> build-modules
             (mapcat :sources)
             (map #(get-in state [:sources %]))
             (filter :foreign)
             (filter :externs-source)
             (map (fn [{:keys [js-name externs externs-source] :as foreign-src}]
                    (SourceFile/fromCode (str "EXTERNS/" js-name) externs-source)))
             ;; just to force the logging
             (into []))]

    (->> (concat default-externs foreign-externs manual-externs)
         (into []))
    ))


;; added by default in init-state
(defn closure-add-replace-constants-pass [cc ^CompilerOptions co]
  (.addCustomPass co CustomPassExecutionTime/BEFORE_CHECKS (ReplaceCLJSConstants. cc)))

(defn add-closure-configurator
  "adds a closure configurator 2-arity function that will be called before the compiler is invoked
   signature of the callback is (fn [compiler compiler-options])

   Compiler and CompilerOptions are mutable objects, the return value of the callback is ignored

   CLJS default configuration is done first, all configurators are applied later and may override
   any options.

   See:
   com.google.javascript.jscomp.Compiler
   com.google.javascript.jscomp.CompilerOptions"
  [state callback]
  (update state :closure-configurators conj callback))

(defn closure-optimize
  "takes the current defined modules and runs it through the closure optimizer

   will return the state with :optimized a list of module which now have a js-source and optionally source maps
   nothing is written to disk, use flush-optimized to write"
  ([state optimizations]
   (-> state
       (assoc :optimizations optimizations)
       (closure-optimize)))
  ([{:keys [build-modules closure-configurators] :as state}]
   (when-not (seq build-modules)
     (throw (ex-info "optimize before compile?" {})))

   (with-logged-time
     [state {:type :optimize}]

     (let [modules
           (make-closure-modules state build-modules)

           ;; can't use the shared one, that only allows one compile
           cc
           (make-closure-compiler)

           co
           (closure/make-options state)

           ;; (fn [closure-compiler compiler-options])
           _ (doseq [cfg closure-configurators]
               (cfg cc co))

           source-map?
           (boolean (:source-map state))

           externs
           (load-externs state)

           state
           (assoc state :build-externs externs)

           result
           (.compileModules cc externs (map :js-module modules) co)]

       (let [warnings (into [] (.warnings result))]
         (when (seq warnings)
           (log state {:type :closure-warnings
                       :warnings warnings})))

       (let [errors (into [] (.errors result))]
         (when (seq errors)
           (log state {:type :closure-errors
                       :errors errors})))

       (assoc state
         :optimized (when (.success result)
                      (let [source-map (when source-map?
                                         (.getSourceMap cc))]

                        (vec (for [{:keys [js-name js-module sources] :as m} modules
                                   ;; reset has to be called before .toSource
                                   :let [_
                                         (when source-map?
                                           (.reset source-map))

                                         output
                                         (.toSource cc js-module)]]
                               (-> m
                                   (dissoc :js-module)
                                   (merge {:output output}
                                     (when source-map?
                                       (let [sw
                                             (StringWriter.)

                                             source-map-name
                                             (str js-name ".map")

                                             _ (.appendTo source-map sw source-map-name)

                                             closure-json
                                             (.toString sw)]

                                         ;; FIXME: not trying to map JS names back to CLJS
                                         ;; (cljs-source-map-for-module state m sources closure-json)
                                         {:source-map-json closure-json
                                          :source-map-name source-map-name})
                                       ))))))))))))


(defn- ns-list-string [coll]
  (->> coll
       (map #(str "'" (comp/munge %) "'"))
       (str/join ",")))

(defn closure-goog-deps
  ([state]
    (closure-goog-deps state (-> state :sources keys)))
  ([state source-names]
   (->> source-names
        (map #(get-in state [:sources %]))
        (map (fn [{:keys [js-name require-order provides]}]
               (str "goog.addDependency(\"" js-name "\","
                    "[" (ns-list-string provides) "],"
                    "[" (ns-list-string require-order) "]);")))
        (str/join "\n"))))

(defn flush-sources-by-name
  ([state]
   (flush-sources-by-name state (mapcat :sources (:build-modules state))))
  ([{:keys [public-dir cljs-runtime-path node-global-prefix] :as state} source-names]
   (with-logged-time
     [state {:type :flush-sources
             :source-names source-names}]
     (doseq [{:keys [type name input last-modified] :as src}
             (->> source-names
                  (map #(get-in state [:sources %])))
             :let [target (io/file public-dir cljs-runtime-path name)]

             ;; skip files we already have since source maps are kinda expensive to generate
             :when (or (not (.exists target))
                       (zero? last-modified)
                       (> (or (:compiled-at src) ;; js is not compiled but maybe modified
                              last-modified)
                          (.lastModified target)))]

       (io/make-parents target)

       (case type
         ;; cljs needs to flush the generated .js, for source-maps also the .cljs and a .map
         :cljs
         (do (let [{:keys [requires provides source-map js-name output]} src
                   js-target (io/file public-dir cljs-runtime-path js-name)]

               (when (nil? output)
                 (throw (ex-info (format "no output for resource: %s" js-name) src)))

               (let [[output sm-offset]
                     (if (= node-global-prefix "global")
                       [output 0]

                       ;; rewrite output to add local names
                       ;; first line should be goog.provide which must be moved
                       (let [namespaces
                             (into requires provides)

                             local-names
                             (util/js-for-local-names namespaces)

                             ;; FIXME: assumes goog.provide is first line which is usually is
                             [provide & lines]
                             (str/split output #"\n")]
                         [(str "var goog = " node-global-prefix ".goog;\n"
                               ;; must call the provide before creating local name
                               provide "\n"
                               ;; lookup global names, make local vars
                               (str/join "\n" local-names)
                               "\n"
                               ;; rest of normal output
                               (str/join "\n" lines))
                          ;; source-map must be aware that we added some lines
                          (-> local-names (count) (inc))]))]

                 (spit js-target output)

                 ;; source-map on the resource is always generated
                 ;; only output it though if globally activated
                 (when (and source-map (:source-map state))
                   (let [source-map-name (str js-name ".map")]
                     (spit (io/file public-dir cljs-runtime-path source-map-name)
                       (sm/encode {name source-map} {:file js-name
                                                     :preamble-line-count sm-offset}))
                     (spit js-target (str "//# sourceMappingURL=" (file-basename source-map-name)) :append true)))))

             ;; spit original source, cljs needed for source maps
             (spit target @input))

         ;; js just needs itself
         ;; FIXME: needs to flush more when js processing is added
         :js
         (do (spit target (:output src))
             ;; foreign libs should play nice with goog require/import and tell what they provide
             (when (:foreign src)
               (spit target (make-foreign-js-source src) :append true)
               ))

         (throw (ex-info "cannot flush" (dissoc src :input :output)))
         ))

     state)))

(defn directory? [^File x]
  (and (instance? File x)
       (or (not (.exists x))
           (.isDirectory x))))

;; FIXME: this could inline everything from a jar since they will never be live-reloaded
;; but it would need to create a proper source-map for the module file
;; since we need that for CLJS files
;; compact-mode has a bunch of work for index source-maps, should be an easy port
(defn inlineable? [{:keys [type from-jar provides requires] :as src}]
  ;; only inline things from jars
  (and from-jar
       ;; only js is inlineable since we want proper source maps for cljs
       (= :js type)
       ;; only inline goog for now
       (every? #(str/starts-with? (str %) "goog.") requires)
       (every? #(str/starts-with? (str %) "goog.") provides)
       ))

(defn flush-unoptmized-module!
  [{:keys [dev-inline-js public-dir public-path unoptimizable] :as state}
   {:keys [default js-name name prepend prepend-js append-js sources web-worker] :as mod}]
  (let [inlineable-sources
        (if-not dev-inline-js
          []
          (->> sources
               (map #(get-in state [:sources %]))
               (filter inlineable?)
               (into [])))

        inlineable-set
        (into #{} (map :name) inlineable-sources)

        target
        (io/file public-dir js-name)

        inlined-js
        (->> inlineable-sources
             (map :output)
             (str/join "\n"))

        inlined-provides
        (->> inlineable-sources
             (mapcat :provides)
             (into #{}))

        ;; goog.writeScript_ (via goog.require) will set these
        ;; since we skip these any later goog.require (that is not under our control, ie REPL)
        ;; won't recognize them as loaded and load again
        closure-require-hack
        (->> inlineable-sources
             (map :js-name)
             (map (fn [js]
                    ;; not entirely sure why we are setting the full path and just the name
                    ;; goog seems to do that
                    (str "goog.dependencies_.written[\"" js "\"] = true;\n"
                         "goog.dependencies_.written[\"" public-path "/" js "\"] = true;")
                    ))
             (str/join "\n"))

        requires
        (->> sources
             (remove inlineable-set)
             (mapcat #(get-in state [:sources % :provides]))
             (distinct)
             (remove inlined-provides)
             (map (fn [ns]
                    (str "goog.require('" (comp/munge ns) "');")))
             (str/join "\n"))

        out
        (str prepend prepend-js inlined-js closure-require-hack requires append-js)

        out
        (if (or default web-worker)
          ;; default mod needs closure related setup and goog.addDependency stuff
          (str unoptimizable
               "var SHADOW_MODULES = {};\n"
               (when web-worker
                 "\nvar CLOSURE_IMPORT_SCRIPT = function(src) { importScripts(src); };\n")
               (closure-defines-and-base state)
               (closure-goog-deps state)
               "\n\n"
               out)
          ;; else
          out)

        out
        (str out "\n\nSHADOW_MODULES[" (pr-str (str name)) "] = true;\n")]

    (spit target out)))


(defn flush-unoptimized!
  [{:keys [build-modules public-dir] :as state}]
  {:pre [(directory? public-dir)]}

  (when-not (seq build-modules)
    (throw (ex-info "flush before compile?" {})))

  (flush-sources-by-name state (mapcat :sources build-modules))

  (with-logged-time
    [state {:type :flush-unoptimized}]

    (flush-manifest public-dir build-modules)

    (doseq [mod build-modules]
      (flush-unoptmized-module! state mod))

    state
    ))

(defn flush-unoptimized
  [state]
  "util for ->"
  (flush-unoptimized! state)
  state)

(defn line-count [text]
  (with-open [rdr (io/reader (StringReader. text))]
    (count (line-seq rdr))))

(defn create-index-map
  [{:keys [public-dir cljs-runtime-path] :as state}
   out-file
   init-offset
   {:keys [sources js-name] :as mod}]
  (let [index-map
        (reduce
          (fn [src-map src-name]
            (let [{:keys [type output js-name] :as rc} (get-in state [:sources src-name])
                  source-map-file (io/file public-dir cljs-runtime-path (str js-name ".map"))
                  lc (line-count output)
                  start-line (:current-offset src-map)

                  ;; extra 2 lines per file
                  ;; // SOURCE comment
                  ;; goog.dependencies_.written[src] = true;
                  src-map (update src-map :current-offset + lc 2)]

              (if (and (= :cljs type)
                       (.exists source-map-file))
                (update src-map :sections conj {:offset {:line (+ start-line 3) :column 0}
                                                ;; :url (str js-name ".map")
                                                ;; chrome doesn't support :url
                                                ;; see https://code.google.com/p/chromium/issues/detail?id=552455
                                                ;; FIXME: inlining the source-map is expensive due to excessive parsing
                                                ;; could try to insert MARKER instead and str/replace
                                                ;; 300ms is acceptable for now, but might not be on bigger projects
                                                ;; flushing the unoptmized version should not exceed 100ms
                                                :map (let [sm (json/read-str (slurp source-map-file))]
                                                       ;; must set sources and file to complete relative paths
                                                       ;; as the source map only contains local references without path
                                                       (assoc sm
                                                         "sources" [src-name]
                                                         "file" js-name))
                                                })
                ;; only have source-maps for cljs
                src-map)
              ))
          {:current-offset init-offset
           :version 3
           :file (str "../" js-name)
           :sections []}
          sources)

        index-map (dissoc index-map :current-offset)]

    ;; (pprint index-map)
    (spit out-file (json/write-str index-map))
    ))

(defn flush-unoptimized-compact
  [{:keys [build-modules public-dir unoptimizable cljs-runtime-path] :as state}]
  {:pre [(directory? public-dir)]}

  (when-not (seq build-modules)
    (throw (ex-info "flush before compile?" {})))

  (flush-sources-by-name state (mapcat :sources build-modules))

  (with-logged-time
    [state {:type :flush-unoptimized
            :compact true}]

    (flush-manifest public-dir build-modules)

    ;; flush fake modules
    (doseq [{:keys [default js-name name prepend prepend-js append-js sources web-worker] :as mod} build-modules]
      (let [target (io/file public-dir js-name)
            append-to-target
            (fn [text]
              (spit target text :append true))]

        (spit target (str prepend prepend-js))
        (when (or default web-worker)
          (append-to-target
            (str unoptimizable
                 "var SHADOW_MODULES = {};\n"
                 (if web-worker
                   "\nvar CLOSURE_IMPORT_SCRIPT = function(src) { importScripts(src); };\n"
                   ;; FIXME: should probably throw an error because we NEVER want to import anything this way
                   "\nvar CLOSURE_IMPORT_SCRIPT = function(src, opt_sourceText) { console.log(\"BROKEN IMPORT\", src); };\n"
                   )
                 (closure-defines-and-base state)
                 (closure-goog-deps state)
                 "\n\n"
                 )))

        ;; at least line-count must be captured here
        ;; since it is the initial offset before we actually have a source map
        (create-index-map
          state
          (io/file public-dir cljs-runtime-path (str (clojure.core/name name) "-index.js.map"))
          (line-count (slurp target))
          mod)

        (doseq [src-name sources
                :let [{:keys [output name js-name] :as rc} (get-in state [:sources src-name])]]
          (append-to-target (str "// SOURCE=" name "\n"))
          ;; pretend we actually loaded a separate file, live-reload needs this
          (append-to-target (str "goog.dependencies_.written[" (pr-str js-name) "] = true;\n"))
          (append-to-target (str (str/trim output) "\n")))

        (append-to-target append-js)
        (append-to-target (str "\n\nSHADOW_MODULES[" (pr-str (str name)) "] = true;\n"))

        (append-to-target (str "//# sourceMappingURL=" cljs-runtime-path "/" (clojure.core/name name) "-index.js.map?r=" (rand)))
        )))

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

(defn reset-resource-by-name [state name]
  (let [{:keys [^File file] :as rc} (get-in state [:sources name])]
    ;; only resource that have a file associated with them can be reloaded (?)
    (if (nil? file)
      state
      (let [new-rc
            (-> rc
                (dissoc :ns :ns-info :requires :require-order :provides :output :compiled :compiled-at)
                (reload-source)
                (as-> src'
                  (inspect-resource state src'))
                (cond-> file
                  (assoc :last-modified (.lastModified file))))]
        (-> state
            (unmerge-resource name)
            (merge-resource new-rc))))
    ))

(defn find-dependent-names
  [state ns-sym]
  (->> (:sources state)
       (vals)
       (filter (fn [{:keys [requires]}]
                 (contains? requires ns-sym)))
       (map :name)
       (into #{})
       ))

(defn find-dependents-for-names [state source-names]
  (->> source-names
       (map #(get-in state [:sources % :provides]))
       (reduce set/union)
       (map #(find-dependent-names state %))
       (reduce set/union)
       (into #{})))

(defn find-resources-using-macro
  "returns a set of names using the macro ns"
  [state macro-ns]
  (let [direct-dependents
        (->> (:sources state)
             (vals)
             (filter (fn [{:keys [macro-namespaces] :as rc}]
                       (contains? macro-namespaces macro-ns)))
             (map :name)
             (into #{}))]

    ;; macro has a companion .cljs file
    ;; FIXME: should check if that file actually self references
    (if (get-resource-for-provide state macro-ns)
      (-> (find-dependent-names state macro-ns)
          (set/union direct-dependents))
      direct-dependents
      )))

(defn reset-resources-using-macro [state macro-ns]
  (let [names (find-resources-using-macro state macro-ns)]
    (reduce reset-resource-by-name state names)
    ))

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

  returns a seq of resource maps with a :scan key which is either :modified :delete

  modified macros will cause all files using to to be returned as well
  although the files weren't modified physically the macro output may have changed"
  [{:keys [sources macros] :as state}]
  (let [reloadable-paths (get-reloadable-source-paths state)]

    ;; FIXME: separate macro scanning from normal scanning
    (let [modified-macros
          (->> macros
               (vals)
               (filter :file)
               (reduce
                 (fn [result {:keys [ns file last-modified] :as macro}]
                   (let [new-mod (.lastModified file)]
                     (if (<= new-mod last-modified)
                       result
                       (let [macro (assoc macro
                                     :scan :macro
                                     :last-modified new-mod)]

                         (conj result macro)))))
                 []))

          affected-by-macros
          (->> modified-macros
               (map :ns)
               (map #(find-resources-using-macro state %))
               (reduce set/union))]

      (->> (vals sources)
           (filter #(contains? reloadable-paths (:source-path %)))
           (filter :file)
           (reduce
             (fn [result {:keys [name ^File file last-modified] :as rc}]
               (cond
                 (not (.exists file))
                 (conj result (assoc rc :scan :delete))

                 (contains? affected-by-macros name)
                 (conj result (assoc rc :scan :modified))

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
  [{:keys [sources] :as initial-state}]
  (log initial-state {:type :waiting-for-modified-files})
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

(defn reload-modified-resource
  [state {:keys [scan name file ns] :as rc}]
  (log state {:type :reload
              :action scan
              :ns ns
              :name name})
  (case scan
    :macro
    (do (try
          ;; FIXME: :reload enough probably?
          (require ns :reload-all)
          (catch Exception e
            (log state {:type :macro-error
                        :ns ns
                        :name name
                        :file file
                        :exception e})))
        (assoc-in state [:macros ns] (dissoc rc :scan)))

    :delete
    (unmerge-resource state (:name rc))

    :new
    (merge-resource state (inspect-resource state (dissoc rc :scan)))

    :modified
    (let [dependents (find-dependent-names state ns)]
      ;; modified files also trigger recompile of all its dependents
      (reduce reset-resource-by-name state (cons name dependents))
      )))

(defn reload-modified-files!
  [state scan-results]
  (as-> state $state
    (reduce reload-modified-resource $state scan-results)
    ;; FIXME: this is kinda ugly but need a way to discover newly required macros
    (discover-macros $state)
    ))

(defn wait-and-reload!
  "wait for modified files, reload them and return reloaded state"
  [state]
  (->> (wait-for-modified-files! state)
       (reload-modified-files! state)))

;; configuration stuff
(defn ^{:deprecated "moved to a closure pass, always done on closure optimize"}
enable-emit-constants [state]
  state)

(defn enable-source-maps [state]
  (assoc state :source-map "cljs.closure/make-options expects a string but we dont use it"))

(defn set-build-options [state opts]
  (merge state opts))

(defn merge-build-options [state opts]
  (reduce-kv
    (fn [state key value]
      (cond
        (and (map? value)
             (map? (get state key)))
        (update state key merge value)

        :default
        (assoc state key value)
        ))
    state
    opts))

(defn get-closure-compiler [state]
  (::cc state))

(defn timing-prefix [{:keys [depth timing duration] :as event} msg]
  (str (when (= :exit timing) "<-")
       (str/join (repeat depth "-") "")
       (when (= :enter timing) "->")
       " "
       msg
       (when (= :exit timing)
         (format " (%d ms)" duration))))

(defn init-state []
  (-> {::is-compiler-state true

       :ignore-patterns
       #{#"^node_modules/"
         #"^goog/demos/"
         #".aot.js$"
         #"_test.js$"
         #"^public/"}

       :classpath-excludes
       [#"resources(/?)$"
        #"classes(/?)$"
        #"java(/?)$"]

       ::cc (make-closure-compiler)

       :closure-warnings
       {:check-types :off}

       :runtime {:print-fn :console}
       :macros-loaded #{}
       :use-file-min true

       :bundle-foreign :inline
       :dev-inline-js true

       :static-fns true
       :elide-asserts false

       :ns-aliases
       '{clojure.pprint cljs.pprint
         clojure.spec cljs.spec}

       ;; (fn [compiler-state ns] alias-ns|nil)
       ;; CLJS by default aliases ALL clojure.* namespaces to cljs.* if the cljs.* version exist
       ;; I prefer a static map since it may be extended by the user and avoids touching the filesystem
       :ns-alias-fn
       (fn [{:keys [ns-aliases] :as state} ns]
         (get ns-aliases ns))

       :closure-configurators []

       ;; :none supprt files are placed into <public-dir>/<cljs-runtime-path>/cljs/core.js
       ;; this used to be just "src" but that is too generic and easily breaks something
       ;; if public-dir is equal to the current working directory
       :cljs-runtime-path "cljs-runtime"

       :manifest-cache-dir
       (let [dir (io/file "target" "shadow-build" "jar-manifest" "v3")]
         (io/make-parents dir)
         dir)

       :cache-dir (io/file "target" "shadow-build" "cljs-cache")
       :cache-level :all

       :public-dir (io/file "public" "js")
       :public-path "js"

       :node-global-prefix "global"

       :optimizations :none
       :n-compile-threads (.. Runtime getRuntime availableProcessors)

       :source-paths {}
       :closure-defines
       {"goog.DEBUG" false
        "goog.LOCALE" "en"}

       :logger
       (let [log-lock (Object.)]
         (reify log/BuildLog
           (log* [_ state evt]
             (locking log-lock
               (let [s (log/event->str evt)
                     s (if (contains? evt :timing)
                         (timing-prefix evt s)
                         s)]
                 (println s))))))}

      (add-closure-configurator closure-add-replace-constants-pass)
      ))

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


