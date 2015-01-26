(ns shadow.cljs.build
  (:import [java.io File StringWriter]
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
            [cljs.util :as util]
            [clojure.core.reducers :as r]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.core.reducers :as r]
            [loom.graph :as lg]
            [loom.alg :as la]
            [clojure.java.shell :as shell]
            ))

(defn ^com.google.javascript.jscomp.Compiler make-closure-compiler []
  (com.google.javascript.jscomp.Compiler/setLoggingLevel Level/WARNING)
  (doto (com.google.javascript.jscomp.Compiler.)
    ;; the thread lingers and prevents the JVM from exiting
    ;; haven't found a clean way to shut it down otherwise
    ;; but given that only one thread is used to compile anyways there
    ;; is really no gain to running in another thread?
    (.disableThreads)))


(defprotocol BuildLog
  (log-warning [this log-string])
  (log-progress [this log-string])
  (log-time-start [this log-string])
  (log-time-end [this log-string time-in-ms]))

(def ^:dynamic *time-depth* 0)

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

(def cljs-warnings (atom {})) ;; I don't like global vars :(

(defn warning-handler
  "saves all warnings into the global map with src-name as key, will be printed after compile-modules"
  [src-name warning-type {:keys [line] :as env} extra]
  (when (get ana/*cljs-warnings* warning-type)
    (when-let [s (ana/error-message warning-type extra)]
      (let [msg (if line
                  (format "%s:%d -> %s" src-name line s)
                  s)]
        (swap! cljs-warnings update-in [src-name] (fnil conj []) msg)
        ))))

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
  (.endsWith (str/lower-case name) ".cljs"))

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

(defn add-goog-dependencies [{:keys [name js-source] :as rc} config]
  (let [deps (-> (JsFileParser. (.getErrorManager (::cc config)))
                 (.parseFile name name js-source))]
    (assoc rc
      :requires (list->ns-set (.getRequires deps))
      :provides (list->ns-set (.getProvides deps)))))

(defn requires-from-ns-ast
  [{:keys [emit-constants] :as state} {:keys [name requires uses]}]
  (let [req (set (vals (merge uses requires)))]
    (if (= 'cljs.core name)
      req
      (if emit-constants
        (conj req 'cljs.core 'constants-table 'runtime-setup)
        (conj req 'cljs.core 'runtime-setup)
        ))))

(defn macros-from-ns-ast [state {:keys [require-macros use-macros]}]
  (into #{} (concat (vals require-macros) (vals use-macros))))

(defn update-rc-from-ns [rc ast state]
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
  [{:keys [name source source-path] :as rc} {:keys [logger] :as state}]
  (let [eof-sentinel (Object.)
        in (readers/indexing-push-back-reader source 1 name)
        rc (ana/with-warning-handlers
             [(partial warning-handler name)]
             (binding [*ns* (create-ns 'cljs.user)
                       ana/*cljs-ns* 'cljs.user
                       ana/*cljs-file* name
                       ana/*analyze-deps* false
                       ana/*load-macros* false
                       ana/*passes* []
                       reader/*data-readers* tags/*cljs-data-readers*]

               (try
                 (let [peek (reader/read in nil eof-sentinel)
                       ast (ana/analyze (ana/empty-env) peek)]

                   (if-not (= :ns (:op ast))
                     (do (log-warning logger (format "Missing NS %s/%s (found %s)" source-path name (:op ast)))
                         rc)
                     (update-rc-from-ns rc ast state)
                     ))
                 (catch ExceptionInfo e
                   (log-warning logger (format "NS form of %s/%s can't be parsed: %s" source-path name (.getMessage e)))
                   (.printStackTrace e)
                   rc))))]

    ;; clear warnings since we may have parsed namespaces we are not going to use
    ;; warnings will re-appear once we actually use a namespace
    (swap! cljs-warnings dissoc name)
    rc))

(defn inspect-resource
  [{:keys [name source url] :as rc} config]
  (cond
    (is-js-file? name)
    (-> rc
        (assoc :type :js
               :js-source source
               :js-name name)
        (add-goog-dependencies config))

    (is-cljs-file? name)
    (-> rc
        (assoc :type :cljs
               :js-name (str/replace name #"\.cljs$" ".js"))
        (peek-into-cljs-resource config))

    :else
    (throw (ex-info "cannot identify as cljs resource" {:name name :url url}))))

(def ^{:doc "windows filenames need to be normalized because they contain backslashes which browsers don't understand"}
     normalize-resource-name
  (if (= File/separatorChar \/)
    identity
    (fn [^String name]
      (str/replace name File/separatorChar \/))))

(defn find-jar-resources [path config]
  (let [file (io/file path)
        abs-path (.getAbsolutePath file)
        jar-file (JarFile. file)
        last-modified (.lastModified file)
        entries (.entries jar-file)]
    (loop [result (transient [])]
      (if (not (.hasMoreElements entries))
        (persistent! result)
        (let [^JarEntry jar-entry (.nextElement entries)
              name (.getName jar-entry)]
          (if (or (not (is-cljs-resource? name))
                  (.startsWith name "goog/demos/")
                  (.endsWith name "_test.js"))
            (recur result)
            (let [url (URL. (str "jar:file:" abs-path "!/" name))
                  rc (inspect-resource {:name (normalize-resource-name name)
                                        :jar true
                                        :source-path path
                                        :last-modified last-modified
                                        :url url
                                        :source (with-open [in (.getInputStream jar-file jar-entry)]
                                                  (slurp in))}
                                       config)]

              (recur (conj! result rc)))
            ))))))

(defn find-fs-resources [^String path config]
  (let [root (io/file path)
        root-path (.getAbsolutePath root)
        root-len (inc (count root-path))]
    (for [file (file-seq root)
          :let [abs-path (.getAbsolutePath file)]
          :when (and (is-cljs-resource? abs-path)
                     (not (.isHidden file)))
          :let [url (.toURL (.toURI file))]]

      (inspect-resource
        {:name (-> abs-path
                   (.substring root-len)
                   (normalize-resource-name))
         :file file
         :source-path path
         :last-modified (.lastModified file)
         :url url
         :source (slurp file)}
        config)
      )))

(defn- do-find-resources-in-paths [config paths]
  (->> paths
       (mapcat (fn [path]
                 (if (.endsWith path ".jar")
                   (find-jar-resources path config)
                   (find-fs-resources path config))))
       (filter usable-resource?)
       (into [])))

(defn- get-deps-for-ns* [state ns-sym]
  (let [name (get-in state [:provide->source ns-sym])]
    (when-not name
      (throw (ex-info (format "ns \"%s\" not available" ns-sym) {:ns ns-sym})))

    (if (contains? (:deps-visited state) name)
      state
      (let [requires (get-in state [:sources name :requires])]
        (when-not requires
          (throw (ex-info "cannot find required namespace deps" {:ns ns-sym :name name})))

        (let [state (conj-in state [:deps-visited] name)
              state (reduce get-deps-for-ns* state requires)]
          (conj-in state [:deps-ordered] name)
          )))))

(defn get-deps-for-ns
  "returns names of all required sources for a given ns (in dependency order), does include self
   (eg. [\"goog/string/string.js\" \"cljs/core.cljs\" \"my-ns.cljs\"])"
  [state ns-sym]
  (-> state
      (assoc :deps-ordered []
             :deps-visited #{})
      (get-deps-for-ns* ns-sym)
      :deps-ordered))

(defn compile-cljs-string
  [state cljs-source name]
  (let [eof-sentinel (Object.)
        in (readers/indexing-push-back-reader cljs-source 1 name)
        source-map? (:source-map state)]

    ;; this logging is a hack, no way arround a global though :(
    (swap! cljs-warnings dissoc name)
    (ana/with-warning-handlers
      [(partial warning-handler name)]

      (binding [comp/*source-map-data* (when source-map?
                                         (atom {:source-map (sorted-map)
                                                :gen-col 0
                                                :gen-line 0}))]

        (let [result
              (loop [{:keys [ns ns-info] :as compile-state} {:js "" :ns 'cljs.user}] ;; :ast []
                ;; FIXME: work arround the fact that *analyze-deps* false also disables warnings
                ;; wait for these to resolve
                ;; http://dev.clojure.org/jira/browse/CLJS-955
                ;; http://dev.clojure.org/jira/browse/CLJS-948
                (let [form (with-redefs [ana/analyze-deps (fn i-dont-analyze-anything
                                                            ([_ _ _])
                                                            ([_ _ _ _]))]
                             (binding [*ns* (create-ns ns)
                                       ana/*cljs-ns* ns
                                       ;; don't actually want to analyze deps, just want the warnings
                                       ana/*analyze-deps* true
                                       ana/*cljs-file* name
                                       reader/*data-readers* tags/*cljs-data-readers*
                                       reader/*alias-map* (merge reader/*alias-map*
                                                                 (:requires ns-info)
                                                                 (:require-macros ns-info))]
                               (reader/read in nil eof-sentinel)))]
                  (if (identical? form eof-sentinel)
                    ;; eof
                    compile-state
                    ;; analyze, concat, recur
                    (recur (binding [*ns* (create-ns ns)
                                     ana/*cljs-ns* ns
                                     ana/*cljs-file* name
                                     ana/*analyze-deps* false
                                     ana/*passes* [ana/infer-type]]

                             (let [ast (ana/analyze (ana/empty-env) form)
                                   ast-js (with-out-str
                                            (comp/emit ast))

                                   compile-state (if (= :ns (:op ast))
                                                   (update-rc-from-ns compile-state ast state)
                                                   compile-state)
                                   compile-state (update-in compile-state [:js] str ast-js)]
                               compile-state
                               ))))))]

          (when-not (:ns result)
            (throw (ex-info "cljs file did not provide a namespace" {:file name})))

          (if source-map?
            (assoc result :source-map (:source-map @comp/*source-map-data*))
            result
            ))))))

(defn compile-cljs-seq
  [state cljs-forms name]

  ;; this logging is a hack, no way arround a global though :(
  (swap! cljs-warnings dissoc name)
  (ana/with-warning-handlers
    [(partial warning-handler name)]

    (let [result (reduce (fn [{:keys [ns] :as compile-state} form]
                           (binding [*ns* (create-ns ns)
                                     ana/*cljs-ns* ns
                                     ana/*cljs-file* name
                                     ana/*analyze-deps* false
                                     ana/*passes* [ana/infer-type]]

                             (let [ast (ana/analyze (ana/empty-env) form)
                                   ast-js (with-out-str
                                            (comp/emit ast))

                                   compile-state (if (= :ns (:op ast))
                                                   (update-rc-from-ns compile-state ast state)
                                                   compile-state)]
                               (update-in compile-state [:js] str ast-js)
                               )))
                         {:js "" :ns 'cljs.user}
                         cljs-forms)]

      (when-not (:ns result)
        (throw (ex-info "cljs file did not provide a namespace" {:file name})))

      result)))

(defn do-compile-cljs-resource
  "given the compiler state and a cljs resource, compile it and return the updated resource
   should not touch global state"
  [state {:keys [name source] :as rc}]

  (with-logged-time
    [(:logger state) (format "Compile CLJS: \"%s\"" name)]
    (let [{:keys [js ns requires source-map]} (cond
                                                (string? source)
                                                (compile-cljs-string state source name)
                                                (vector? source)
                                                (compile-cljs-seq state source name)
                                                :else
                                                (throw (ex-info "invalid cljs source type" {:name name :source source})))]
      (assoc rc
        :js-source js
        :requires requires
        :compiled-at (System/currentTimeMillis)
        :provides #{ns}
        :compiled true
        :source-map source-map))))

(defn get-cache-file-for-rc
  [{:keys [cache-dir] :as state} {:keys [name] :as rc}]
  (io/file cache-dir (str name ".cache.edn")))

(defn load-cached-cljs-resource
  [{:keys [logger public-dir] :as state} {:keys [ns js-name name last-modified] :as rc}]
  (let [cache-file (get-cache-file-for-rc state rc)
        target-js (io/file public-dir "src" js-name)]

    (when (and (.exists cache-file)
               (> (.lastModified cache-file) last-modified)
               (.exists target-js)
               (> (.lastModified target-js) last-modified)

               ;; only use cache if its older than anything it depends on
               (let [min-age (->> (get-deps-for-ns state ns)
                                  (map #(get-in state [:sources % :last-modified]))
                                  #_ (map (fn [{:keys [name last-modified] :as src}]
                                         (prn [:last-mod name last-modified])
                                         last-modified))
                                  (remove nil?) ;; might not have :last-modified (eg. runtime-setup)
                                  (reduce (fn [a b] (Math/max a b))))]
                 (> (.lastModified cache-file) min-age)))

      (let [cache-data (edn/read-string (slurp cache-file))]

        (when (= (util/clojurescript-version) (:version cache-data))
          (log-progress logger (format "Load cached cljs resource \"%s\"" name))

          ;; restore analysis data
          (swap! env/*compiler* assoc-in [::ana/namespaces (:ns cache-data)] (:analyzer cache-data))

          ;; merge resource data & return it
          ;; FIXME: lost source-map data at this point, assuming it exist when out-js exists
          (-> (merge rc cache-data)
              (dissoc :analysis :version)
              (assoc :js-source (slurp target-js))))))))

(defn write-cached-cljs-resource
  [{:keys [logger] :as state} {:keys [ns name] :as rc}]

  ;; only cache files that don't have warnings!
  (when-not (get @cljs-warnings name)

    (let [cache-file (get-cache-file-for-rc state rc)
          cache-data (-> rc
                         ;; dont write :source-map, assume we generate it later and are able to reuse it
                         (dissoc :file :url :js-source :source :source-map)
                         (assoc :version (util/clojurescript-version)
                                :analyzer (get-in @env/*compiler* [::ana/namespaces ns])))]
      (io/make-parents cache-file)
      (spit cache-file (pr-str cache-data))
      (log-progress logger (format "Wrote cache for \"%s\" to \"%s\"" name cache-file)))))

(defn maybe-compile-cljs
  "take current state and cljs source name to compile
   make sure you are in with-compiler-env"
  [{:keys [cache-dir cache-level] :as state} cljs-name]
  (let [{:keys [jar] :as src} (get-in state [:sources cljs-name])
        cache? (and cache-dir (or (= cache-level :all) (and (= cache-level :jars) jar)))
        src (or (when cache?
                  (load-cached-cljs-resource state src))
                (let [src (do-compile-cljs-resource state src)]
                  (when cache?
                    (write-cached-cljs-resource state src))
                  src))]
    (assoc-in state [:sources cljs-name] src)
    ))

(defn merge-provides [state provided-by provides]
  (if-let [provide (first provides)]
    (recur (assoc-in state [:provide->source provide] provided-by) provided-by (rest provides))
    state
    ))

(defn unmerge-provides [state provides]
  (if-let [provide (first provides)]
    (recur (update-in state [:provide->source] dissoc provide) (rest provides))
    state
    ))

(defn unmerge-resource [state name]
  (if-let [{:keys [provides] :as current} (get-in state [:sources name])]
    (-> state
        (unmerge-provides provides)
        (update-in [:sources] dissoc name))
    ;; else: not present
    state))

(defn merge-resources
  [state [{:keys [name provides] :as src} & more]]
  (if src
    (recur (-> state
               (unmerge-resource name)
               (assoc-in [:sources name] src)
               (merge-provides name provides))
           more)
    ;; else: nothing new left
    state
    ))

;;; COMPILE STEPS

(defn step-find-resources-in-jars
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

(defn step-find-resources
  "finds cljs resources in the given path"
  ([state path]
    (step-find-resources state path {:reloadable true}))
  ([state path opts]
    (with-logged-time
      [(:logger state) (format "Find cljs resources in path: \"%s\"" path)]
      (-> state
          (assoc-in [:source-paths path] (assoc opts
                                           :path path))
          (merge-resources (do-find-resources-in-paths state [path]))
          ))))

(def cljs-core-name "cljs/core.cljs")
(def goog-base-name "goog/base.js")

(defmacro with-compiler-env
  "compiler env is a rather big piece of dynamic state
   so we take it out when needed and put the updated version back when done
   doesn't carry the atom arround cause the compiler state itself should be persistent
   thus it should provide safe points

   the body should yield the updated compiler state and not touch the compiler env

   I don't touch the compiler env itself yet at all, might do for some metadata later"
  [state & body]
  `(let [dyn-env# (atom (:compiler-env ~state))
         new-state# (binding [env/*compiler* dyn-env#]
                      ~@body)]
     (assoc new-state# :compiler-env @dyn-env#)))

(defn ^:deprecated step-compile-core [state]
  ;; honestly not sure why this was ever here
  ;; since we compile in dep order 'cljs.core will always be compiled before any other CLJS
  state)


(defn discover-macros [state]
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
                                     url (io/resource name)]
                                 {:ns macro-ns
                                  :used-by used-by
                                  :name name
                                  :url url})))
                        (map (fn [{:keys [url] :as info}]
                               (if (not= "file" (.getProtocol url))
                                 info
                                 (let [file (io/file (.getPath url))]
                                   (assoc info
                                     :file file
                                     :last-modified (.lastModified file))))))
                        (map (juxt :name identity))
                        (into {}))]
    (assoc state :macros macro-info)
    ))



(defn step-finalize-config [state]
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


(defn resolve-main-deps [{:keys [logger] :as state} main-cljs]
  (log-progress logger (format "Resolving deps for: %s" main-cljs))
  (let [deps (get-deps-for-ns state main-cljs)]
    (assoc-in state [:main-deps main-cljs] deps)))

(defn reset-modules [state]
  (-> state
      (assoc :modules {})
      (dissoc :default-module :main-deps :build-modules)
      ))

(defn step-configure-module
  ([state module-name module-mains depends-on]
    (step-configure-module state module-name module-mains depends-on {}))
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

(defn do-load-compiled-files [state {:keys [public-dir last-modified name js-name] :as src}]
  (let [target (io/file public-dir js-name)]
    (if (and (.exists target)
             (> (.lastModified target) last-modified))
      (let [ns (cljs-file->ns name)]
        (-> state
            (assoc-in [:sources name] (-> src
                                          (assoc :js-source (slurp target))
                                          (add-goog-dependencies state)
                                          (assoc :precompiled true)))
            (assoc-in [:provide->source ns] name)))
      ;; else: no precompiled version available
      state)))

(defn step-load-compiled-files [state]
  (with-logged-time
    [(:logger state) (format "Loading precompiled files ...")]
    (reduce do-load-compiled-files
            state
            ;; only load precompiled cljs where the source has already been discovered
            (->> state
                 :sources
                 vals
                 (filter #(= :cljs (:type %)))
                 (remove :generated)
                 ))))

(defn flush-to-disk
  "flush all generated sources to disk, not terribly useful, use flush-unoptimized to include source maps"
  [{:keys [work-dir sources] :as state}]
  (with-logged-time
    [(:logger state) (format "Flushing to disk")]
    (doseq [{:keys [type name compiled] :as src} (vals sources)
            :when (and (= :cljs type)
                       compiled)]

      (let [{:keys [js-name js-source]} src
            target (io/file work-dir js-name)]
        (io/make-parents target)
        (spit target js-source)))
    state))

(defn step-generate-constants [state]
  (if (not (:emit-constants state))
    state
    (let [constants (with-out-str
                      (comp/emit-constants-table
                        (::ana/constant-table (:compiler-env state))))]
      (update-in state [:sources "constants_table.js"] merge {:source constants
                                                              :js-source constants
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
        goog-base (:js-source goog-rc)]

    (when-not (seq goog-base)
      (throw (ex-info "no goog/base.js" {})))

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
  [{:keys [provides requires js-source]}]
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

                        (doseq [{:keys [name] :as src} (map #(get-in state [:sources %]) sources)]
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
  (doseq [[src-name warnings] (->> @cljs-warnings
                                   (sort-by first)) ;; sort by filename
          ]
    (log-warning logger (format "WARNINGS: %s (%d)" src-name (count warnings)))
    (doseq [msg warnings]
      (log-warning logger msg)))
  state)

(defn do-analyze-module
  "resolve all deps for a given module, based on specified :mains
   will update state for each module with :sources, a list of sources needed to compile this module "
  [state {:keys [name mains] :as module}]
  (let [state (reduce resolve-main-deps state mains)
        module-deps (->> mains
                         (mapcat #(get-in state [:main-deps %]))
                         (distinct))]
    (assoc-in state [:modules name :sources] module-deps)))

(defn add-foreign
  [state name provides requires js-source]
  {:pre [(string? name)
         (set? provides)
         (seq provides)
         (set? requires)
         (string? js-source)]}

  (merge-resources state [{:type :js
                           :foreign true
                           :name name
                           :js-name name
                           :provides provides
                           :requires requires
                           :js-source js-source
                           :source js-source
                           }]))

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
     ;; FIXME: why do I need to set both?
     :js-source src
     :source src}))

(defn require-macros!
  [{:keys [logger macros-loaded] :as state} used-sources]
  (let [macros-to-load (->> used-sources
                            (mapcat (fn [{:keys [ns-info]}]
                                      (set (concat (vals (:require-macros ns-info))
                                                   (vals (:use-macros ns-info))))))
                            (remove macros-loaded)
                            (into #{}))]
    (reduce (fn [state macro-ns]
              (log-progress logger (format "Require Macro NS: %s" macro-ns))
              (require macro-ns)
              (update-in state [:macros-loaded] conj macro-ns))
            state
            macros-to-load)))



(defn step-compile-modules [state]
  (with-logged-time
    [(:logger state) "Compiling Modules ..."]
    (let [state (merge-resources state [(make-runtime-setup state)])
          state (reduce do-analyze-module state (-> state :modules (vals)))

          modules (sort-and-compact-modules state)

          sources (->> modules
                       (mapcat :sources)
                       (map #(get-in state [:sources %]))
                       (filter #(= :cljs (:type %))))

          state (require-macros! state sources)

          source-names (->> sources
                            (remove :compiled)
                            (map :name))

          state (with-compiler-env state
                  (reduce maybe-compile-cljs state source-names))]

      (-> state
          (assoc :build-modules modules)
          (step-generate-constants)
          (do-print-warnings)
          ))))

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

  (let [js-mods (reduce (fn [js-mods {:keys [js-name name depends-on sources prepend-js append-js] :as mod}]
                          (let [js-mod (JSModule. js-name)]
                            (when (:default mod)
                              (.add js-mod (SourceFile/fromCode "closure_setup.js" (closure-defines-and-base state))))
                            (when (seq prepend-js)
                              (.add js-mod (SourceFile/fromCode (str "mod_" name "_prepend.js") prepend-js)))

                            (doseq [{:keys [name js-name js-source] :as src} (map #(get-in state [:sources %]) sources)]
                              ;; throws hard to track NPE otherwise
                              (when-not (and js-name js-source (seq js-source))
                                (throw (ex-info "missing js-source for source" {:js-name js-name :name (:name src)})))

                              (if (:foreign src)
                                (.add js-mod (SourceFile/fromCode js-name (make-foreign-js-source src)))
                                (.add js-mod (SourceFile/fromCode js-name js-source))))

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
      (doseq [{:keys [type name source] :as src} (map #(get-in state [:sources %]) sources)]
        (let [target (io/file public-dir "src" name)]
          (io/make-parents target)
          (spit target source))))
    state))

;; FIXME: manifest should be custom step
(defn flush-manifest [public-dir modules]
  (spit (io/file public-dir "manifest.json")
        (json/write-str (map #(select-keys % [:name :js-name :mains :depends-on :default :sources]) modules))))

(defn foreign-js-source-for-mod [state {:keys [sources] :as mod}]
  (->> sources
       (map #(get-in state [:sources %]))
       (filter :foreign)
       (map :js-source)
       (str/join "\n")))

(defn flush-modules-to-disk [{modules :optimized :keys [unoptimizable ^File public-dir public-path logger] :as state}]
  (with-logged-time
    [(:logger state) "Flushing modules to disk"]

    (when-not (seq modules)
      (throw (ex-info "flush before optimize?" {})))

    (when-not public-dir
      (throw (ex-info "missing :public-dir" {})))

    (doseq [{:keys [default js-source prepend source-map-name name js-name] :as mod} modules]
      (let [target (io/file public-dir js-name)
            js-source (if default
                        (str unoptimizable js-source)
                        js-source)
            js-source (str prepend (foreign-js-source-for-mod state mod) js-source)
            js-source (if (:web-worker mod)
                        (let [deps (:depends-on mod)]
                          (str (str/join "\n" (for [other modules
                                                    :when (contains? deps (:name other))]
                                                (str "importScripts('" (:js-name other) "');")))
                               "\n\n"
                               js-source))
                        js-source)]
        (io/make-parents target)
        (spit target js-source)

        (log-progress logger (format "Wrote module \"%s\" (size: %d)" js-name (count js-source)))

        (when source-map-name
          (spit target (str "\n//# sourceMappingURL=src/" (file-basename source-map-name) "\n")
                :append true)))))

  (flush-manifest public-dir modules)

  (when (:source-map state)
    (flush-source-maps state))

  state)



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

          ;; FIXME: that probably wont work with this arch, provide :externs config
          externs (closure/load-externs state)

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
                                        js-source (.toSource cc js-module)]]
                              (-> m
                                  (dissoc :js-module)
                                  (merge {:js-source js-source}
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

(defn closure-goog-deps [{:keys [modules] :as state}]
  (->> (for [src-name (mapcat :sources (vals modules))]
         (let [{:keys [js-name requires provides]} (get-in state [:sources src-name])]
           (str "goog.addDependency(\"" js-name "\","
                "[" (ns-list-string provides) "],"
                "[" (ns-list-string requires) "]);")))
       (str/join "\n")))

(defn flush-sources
  [{:keys [build-modules public-dir public-path unoptimizable] :as state}]
  (when-not (seq build-modules)
    (throw (ex-info "flush before compile?" {})))
  (with-logged-time
    [(:logger state) "Flushing sources"]

    (doseq [{:keys [type name source last-modified] :as src}
            (->> (mapcat :sources build-modules)
                 (map #(get-in state [:sources %])))
            :let [target (io/file public-dir "src" name)]

            ;; skip files we already have since source maps are kinda expensive to generate
            :when (or (not (.exists target))
                      (nil? last-modified) ;; runtime-setup doesn't have last-modified
                      (> (or (:compiled-at src) ;; js is not compiled but maybe modified
                             last-modified)
                         (.lastModified target)))]

      ;; spit original source, cljs needed for source maps
      (io/make-parents target)
      (spit target source)

      ;; also spit js source since for source maps
      (when (= :cljs type)
        (let [{:keys [source-map js-name js-source]} src
              target (io/file public-dir "src" js-name)]

          (spit target js-source)

          (when source-map
            (let [source-map-name (str js-name ".map")]
              (spit (io/file public-dir "src" source-map-name)
                    (sm/encode {name source-map} {}))
              (spit target (str "//# sourceMappingURL=" (file-basename source-map-name) "?r=" (rand)) :append true)))
          ))))
  ;; return unmodified state
  state)

(defn flush-unoptimized
  [{:keys [build-modules public-dir public-path unoptimizable] :as state}]

  (flush-sources state)

  (with-logged-time
    [(:logger state) "Flushing unoptimized modules"]

    (flush-manifest public-dir build-modules)

    ;; flush fake modules
    (doseq [{:keys [default js-name name prepend prepend-js append-js sources] :as mod} build-modules]
      (let [provided-ns (mapcat #(reverse (get-in state [:sources % :provides]))
                                sources)
            target (io/file public-dir js-name)

            out (->> provided-ns
                     (map (fn [ns]
                            (str "goog.require('" (comp/munge ns) "');")))
                     (str/join "\n"))
            out (str prepend prepend-js out append-js)
            out (if default
                  ;; default mod needs closure related setup and goog.addDependency stuff
                  (str unoptimizable
                       (closure-defines-and-base state)
                       (closure-goog-deps state)
                       "\n\n"
                       out)
                  ;; else
                  out)]

        (spit target out))))
  ;; return unmodified state
  state)

(defn flush-unoptimized-node
  [{:keys [build-modules public-dir public-path unoptimizable] :as state}]
  (when (not= 1 (count build-modules))
    (throw (ex-info "node builds can only have one module!" {})))

  (flush-sources state)

  (with-logged-time
    [(:logger state) (format "Flushing node script: %s" (-> build-modules first :js-name))]

    (let [{:keys [default js-name name prepend prepend-js append-js sources] :as mod} (first build-modules)]
      (let [provided-ns (mapcat #(reverse (get-in state [:sources % :provides]))
                                sources)
            target (io/file public-dir js-name)

            out (->> provided-ns
                     (map (fn [ns]
                            (str "goog.require('" (comp/munge ns) "');")))
                     (str/join "\n"))
            out (str prepend prepend-js out append-js)

            out (str (slurp (io/resource "shadow/cljs/node_bootstrap.txt"))
                     "\n\n"
                     out)
            goog-js (io/file public-dir "src" "goog" "base.js")
            deps-js (io/file public-dir "src" "deps.js")]
        (spit goog-js (get-in state [:sources "goog/base.js" :source]))
        (spit deps-js (closure-goog-deps state))

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
  (assoc rc :source (slurp url)))

(defn reset-resource [{:keys [^File file] :as src} config]
  (-> src
      (dissoc :ns :ns-info :requires :provides :js-source :compiled :compiled-at)
      (reload-source)
      (inspect-resource config)
      (cond-> file
              (assoc :last-modified (.lastModified file)))))



(defn do-reload-modified
  "checks if a src was touched and reset the associated state if it was"
  [state {:keys [name type ^File file last-modified] :as src}]
  (if (> (.lastModified file) last-modified)
    (merge-resources state [(reset-resource src state)])
    state))

(defn step-reload-modified
  "just look at all sources if modified"
  [state]
  ;; only reload files in paths marked to be reloadable
  (let [reloadable-paths (get-reloadable-source-paths state)]
    (reduce do-reload-modified state (->> state
                                          :sources
                                          (vals)
                                          (filter :file)
                                          (filter #(contains? reloadable-paths (:source-path %)))
                                          ))))

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
         (mapcat #(find-fs-resources % state))
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
      (assoc :emit-constants true)
      (assoc-in [:compiler-env :opts :emit-constants] true)
      (assoc-in [:sources "constants_table.js"] {:type :js
                                                 :generated true
                                                 :js-name "constants_table.js"
                                                 :name "constants_table.js"
                                                 :provides #{'constants-table}
                                                 :requires #{}})
      (assoc-in [:provide->source 'constants-table] "constants_table.js")))

(defn enable-source-maps [state]
  (assoc state :source-map "cljs.closure/make-options expects a string but we dont use it"))

(defn init-state []
  ;; static fns dont work in repl env, but i never used a cljs repl
  ;; need to look into cljs repl, otherwise I see no downside to using static fns
  (alter-var-root #'ana/*cljs-static-fns* (fn [_] true))

  ;; load cljs.core macros, we are probably going to use them
  (ana/load-core)

  {:compiler-env {} ;; will become env/*compiler*

   ::cc (make-closure-compiler)

   :runtime {:print-fn :console}
   :macros-loaded #{}

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


(defn execute! [{:keys [logger public-dir] :as state} program & args]
  (when (not= 1 (-> state :build-modules count))
    (throw (ex-info "can only execute non modular builds" {})))

  (let [script-name (-> state :build-modules first :js-name)
        script-args (->> args
                         (map (fn [arg]
                                (cond
                                  (string? arg)
                                  arg
                                  (= :script arg)
                                  script-name
                                  :else
                                  (throw (ex-info "invalid execute args" {:args args})))))
                         (into [program]))
        pb (doto (ProcessBuilder. script-args)
             (.directory public-dir)
             (.inheritIO))]

    ;; not using this because we only get output once it is done
    ;; I prefer to see progress
    ;; (prn (apply shell/sh script-args))

    (with-logged-time
      [logger (format "Execute: %s" (pr-str script-args))]
      (let [proc (.start pb)]
        ;; FIXME: what if this doesn't terminate?
        (.waitFor proc))))

  state)

(defn setup-test-runner [state test-namespaces]
  (let [test-runner-src {:name "test_runner.cljs"
                         :js-name "test_runner.js"
                         :type :cljs
                         :provides #{'test-runner}
                         :requires (into #{'cljs.test} test-namespaces)
                         :ns 'test-runner
                         ;; FIXME: there should a better way for this?
                         :source [(list 'ns 'test-runner
                                        (concat
                                          (list :require '[cljs.test])
                                          (mapv vector test-namespaces)))
                                  (concat (list 'cljs.test/run-tests '(cljs.test/empty-env))
                                          (for [it test-namespaces]
                                            `(quote ~it)))]}]
    (-> state
        (merge-resources [test-runner-src])
        (reset-modules)
        (step-configure-module :test-runner ['test-runner] #{}))))

(defn make-test-runner [state test-namespaces]
  (-> state
      (setup-test-runner test-namespaces)
      (assoc-in [:runtime :print-fn] :print)
      (step-compile-modules)
      (flush-unoptimized-node)))

(defn execute-affected-tests!
  [{:keys [logger] :as state} source-names]
  (let [test-namespaces (->> source-names
                             (find-dependent-resources state)
                             (filter #(has-tests? (get-in state [:sources %])))
                             (map #(get-in state [:sources % :ns]))
                             (distinct)
                             (into []))]
    (if (empty? test-namespaces)
      (do (log-progress logger (format "No tests to run for: %s" (pr-str source-names)))
          state)
      (do (-> state
              (make-test-runner test-namespaces)
              (execute! "node" :script))
          ;; return unmodified state, otherwise previous module information and config is lost
          state))))

(defn execute-all-tests! [state]
  (let [test-namespaces (->> (get-in state [:sources])
                             (vals)
                             (remove :jar)
                             (filter has-tests?)
                             (map :ns)
                             (into []))]
    (-> state
        (make-test-runner test-namespaces)
        (execute! "node" :script))

    ;; return unmodified state!
    state
    ))
