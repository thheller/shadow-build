(ns shadow.cljs.build
  (:import [java.io File PrintStream StringWriter StringReader]
           [java.net URL]
           [com.google.javascript.jscomp JSModule SourceFile CompilerOptions$DevMode CompilerOptions$TracerMode]
           (clojure.lang ExceptionInfo))
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
            [shadow.cljs.passes :as passes]
            [loom.graph :as lg]
            [loom.alg :as la]
            ))

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

(defn jar-entry-names [jar-path]
  (with-open [z (java.util.zip.ZipFile. jar-path)]
    (doall (map #(.getName %) (enumeration-seq (.entries z))))))

(defn classpath-entries
  "finds all js files on the classpath matching the path provided"
  []
  (let [sysp (System/getProperty "java.class.path")]
    (if (.contains sysp ";")
      (str/split sysp #";")
      (str/split sysp #":"))))

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

(defn usable-resource? [{:keys [type provides requires] :as rc}]
  (or (= :cljs type) ;; cljs is always usable
      (seq provides) ;; provides something is useful
      (seq requires) ;; requires something is less useful?
      (= "goog/base.js" (:name rc)) ;; doesnt provide/require anything but is useful
      ))

(defn cljs->js-name [name]
  (str/replace name #"\.cljs$" ".js"))

(defn ns->cljs-file [ns]
  (-> ns
      (str)
      (str/replace #"\." "/")
      (str/replace #"-" "_")
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

(defn add-goog-dependencies [{:keys [js-source] :as rc}]
  (->> js-source
       (StringReader.)
       (io/reader)
       (line-seq)
       (mapcat #(re-seq #"^goog\.(provide|require)\(['\"]([^'\"]+)['\"]\)" %))
       (reduce
         (fn [rc [_ x ns]]
           (let [ns (-> ns
                        (str/replace #"_" "-")
                        (symbol))]
             (case x
               "require" (conj-in rc [:requires] ns)
               "provide" (conj-in rc [:provides] ns))))
         (assoc rc
                :requires #{}
                :provides #{}))))

(defn requires-from-ns-ast
  [{:keys [emit-constants] :as state} {:keys [name requires uses]}]
  (let [req (set (vals (merge uses requires)))]
    (if (= 'cljs.core name)
      req
      (if emit-constants
        (conj req 'cljs.core 'constants-table)
        (conj req 'cljs.core)
        ))))

(defn peek-into-cljs-resource
  "looks at the first form in a .cljs file, analyzes it if (ns ...) and returns the updated resource
   with ns-related infos"
  [{:keys [name source source-path] :as rc} {:keys [logger] :as state}]
  (let [eof-sentinel (Object.)
        in (readers/indexing-push-back-reader source 1 name)]
    (ana/with-warning-handlers
      [(partial warning-handler name)]
      (binding [*ns* (create-ns 'cljs.user)
                ana/*cljs-ns* 'cljs.user
                ana/*cljs-file* name
                ana/*analyze-deps* false
                ana/*passes* [ana/infer-type passes/macro-js-requires]
                reader/*data-readers* tags/*cljs-data-readers*]

        (try
          (let [peek (reader/read in nil eof-sentinel)
                ast (ana/analyze (ana/empty-env) peek)]

            (if-not (= :ns (:op ast))
              (do (log-warning logger (format "Missing NS %s/%s (found %s)" source-path name (:op ast)))
                  rc)
              ;; ns form, extract info and update resource
              (let [ns-name (:name ast)
                    requires (requires-from-ns-ast state ast)]
                (assoc rc
                       :ns ns-name
                       :ns-info (dissoc ast :env)
                       :provides #{ns-name}
                       :requires requires))))
          (catch ExceptionInfo e
            (log-warning logger (format "NS form of %s/%s can't be parsed: %s" source-path name (.getMessage e)))
            rc))))))

(defn read-resource
  [{:keys [name url] :as rc} config]
  (cond
    (is-js-file? name)
    (let [source (slurp url)]
      (-> rc
          (assoc :type :js
                 :source source
                 :js-source source
                 :js-name name)
          (add-goog-dependencies)))

    (is-cljs-file? name)
    (-> rc
        (assoc :type :cljs
               :source (slurp url)
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

(defn find-jar-resources [path]
  (let [jar-file (io/file path)
        last-modified (.lastModified jar-file)]
    (for [jar-entry (jar-entry-names path)
          :when (is-cljs-resource? jar-entry)
          :let [url (URL. (str "jar:file:" (.getAbsolutePath jar-file) "!/" jar-entry))]]
      {:name (normalize-resource-name jar-entry)
       :jar true
       :source-path path
       :last-modified last-modified
       :url url})))

(defn find-fs-resources [^String path]
  (let [root (io/file path)
        root-path (.getAbsolutePath root)
        root-len (inc (count root-path))]
    (for [file (file-seq root)
          :let [abs-path (.getAbsolutePath file)]
          :when (and (is-cljs-resource? abs-path)
                     (not (.isHidden file)))]
      {:name (-> abs-path
                 (.substring root-len)
                 (normalize-resource-name))
       :file file
       :source-path path
       :last-modified (.lastModified file)
       :url (.toURL (.toURI file))}
      )))

(defn- do-find-resources-in-paths [config paths]
  (let [resources (->> paths
                       (mapcat (fn [path]
                                 (if (.endsWith path ".jar")
                                   (find-jar-resources path)
                                   (find-fs-resources path))))
                       (vec))]

    ;; (->> resources (map read-resource) (filter usable-resource?))

    ;; REDUCERS FTW!
    ;; whats a good n here? doesn't seem to gain much by going lower on my macpro+ssd
    ;; is almost twice as fast as without reducers though
    ;; biggest bottleneck should be IO
    ;; also that we are re-opening a .jar for every file in it
    ;; so its probably best to read jar files in one go?
    ;; (into [] (r/fold 256 r/cat r/append! (r/filter usable-resource? (r/map #(read-resource % config) resources))))

    ;; cannot use reducers apparantly as it MIGHT cause trouble due to parrallel loading of macro namespaces
    ;; (ns some.ns (:require-macros [macro-ns]))
    ;; (ns other.ns (:require-macros [macro-ns]))
    ;; since read-resource might peek into these files at the same time the cljs.analyzer will require the macro-ns
    ;; either clojure does not have any guards to prevent concurrent require or they don't hold
    ;; one example to encounter this is have 2 jars which both use cljs.core.async.macros
    ;; MIGHT get: clojure.lang.ExceptionInfo: java.lang.RuntimeException: No such var: ioc/state-machine, compiling:(cljs/core/async/macros.clj:18:19)
    ;; if in unfortunate load order, might not happen ...
    (->> resources
         (map #(read-resource % config))
         (filter usable-resource?)
         (into []))))

(defn compile-cljs-string
  [state cljs-source name]
  (let [eof-sentinel (Object.)
        in (readers/indexing-push-back-reader cljs-source 1 name)
        source-map? (:source-map state)]

    ;; this logging is a hack, no way arround a global though :(
    (swap! cljs-warnings dissoc name)
    (ana/with-warning-handlers
      [(partial warning-handler name)]

      ;; FIXME: should assume the ns was already processed when analyzing namespaces
      ;; and skip the ns, nasty bit a code duplication! 

      (binding [comp/*source-map-data* (when source-map?
                                         (atom {:source-map (sorted-map)
                                                :gen-col 0
                                                :gen-line 0}))]

        (let [result
              (loop [{:keys [ns ns-info] :as compile-state} {:js "" :ns 'cljs.user}] ;; :ast []
                (let [;; FIXME: i really dont like bindings ...
                      form (binding [*ns* (create-ns ns)
                                     ana/*cljs-ns* ns
                                     ana/*analyze-deps* false
                                     ana/*cljs-file* name
                                     reader/*data-readers* tags/*cljs-data-readers*
                                     reader/*alias-map* (merge reader/*alias-map*
                                                               (:requires ns-info)
                                                               (:require-macros ns-info))]
                             (reader/read in nil eof-sentinel))]
                  (if (identical? form eof-sentinel)
                    ;; eof
                    compile-state
                    ;; analyze, concat, recur
                    (recur (binding [*ns* (create-ns ns)
                                     ana/*cljs-ns* ns
                                     ana/*cljs-file* name
                                     ana/*analyze-deps* false
                                     ana/*cljs-warnings* (merge ana/*cljs-warnings* {:undeclared-var true
                                                                                     :undeclared-ns true})
                                     ana/*passes* [ana/infer-type passes/macro-js-requires]]

                             (let [ast (ana/analyze (ana/empty-env) form)
                                   ;; TBD: does it make sense to keep the ast arround?
                                   ;; state (update-in state [:ast] ast)
                                   ast-js (with-out-str
                                            (comp/emit ast))

                                   compile-state (if (= :ns (:op ast))
                                                   (let [ns-name (:name ast)
                                                         requires (requires-from-ns-ast state ast)]
                                                     (assoc compile-state :ns ns-name :ns-info (dissoc ast :env) :requires requires))
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

(defn do-compile-cljs-resource
  "given the compiler state and a cljs resource, compile it and return the updated resource
   should not touch global state"
  [state {:keys [name last-modified source] :as rc}]

  (with-logged-time
    [(:logger state) (format "Compile CLJS: \"%s\"" name)]
    (let [{:keys [js ns requires source-map] :as result} (compile-cljs-string state source name)]
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
  [{:keys [logger public-dir] :as state} {:keys [js-name name last-modified source] :as rc}]
  (let [cache-file (get-cache-file-for-rc state rc)
        target-js (io/file public-dir "src" js-name)]

    (when (and (.exists cache-file)
               (> (.lastModified cache-file) last-modified)
               (.exists target-js)
               (> (.lastModified target-js) last-modified))

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

  (let [cache-file (get-cache-file-for-rc state rc)
        cache-data (-> rc
                       ;; dont write :source-map, assume we generate it later and are able to reuse it
                       (dissoc :file :url :js-source :source :source-map)
                       (assoc :version (util/clojurescript-version)
                              :analyzer (get-in @env/*compiler* [::ana/namespaces ns])))]
    (io/make-parents cache-file)
    (spit cache-file (pr-str cache-data))
    (log-progress logger (format "Wrote cache for \"%s\" to \"%s\"" name cache-file))))

(defn maybe-compile-cljs
  "take current state and cljs source name to compile
   make sure you are in with-compiler-env"
  [state cljs-name]
  (let [src (get-in state [:sources cljs-name])
        src (or (when (:cache-dir state)
                  (load-cached-cljs-resource state src))
                (let [src (do-compile-cljs-resource state src)]
                  (when (:cache-dir state)
                    (write-cached-cljs-resource state src))
                  src))]
    (assoc-in state [:sources cljs-name] src)
    ))

(defn merge-provides [state provided-by provides]
  (if-let [provide (first provides)]
    (recur (assoc-in state [:provide-index provide] provided-by) provided-by (rest provides))
    state
    ))

(defn unmerge-provides [state provides]
  (if-let [provide (first provides)]
    (recur (update-in state [:provide-index] dissoc provide) (rest provides))
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
  [{:keys [work-dir] :as state} [{:keys [name provides] :as src} & more]]
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
  [{:keys [source-paths work-dir] :as state}]
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

(defn step-compile-core [state]
  (when-not (:configured state)
    (throw (ex-info "finalize config first" {})))

  (with-logged-time
    [(:logger state) "Compiling cljs.core"]

    (when-not (get-in state [:sources goog-base-name])
      (throw (ex-info (str "couldn't find " goog-base-name) {})))

    (with-compiler-env state
      (let [cljs-core (get-in state [:sources cljs-core-name])]
        (when-not cljs-core
          (throw (ex-info (str "couldn't find " cljs-core-name) {})))
        (-> state
            (maybe-compile-cljs cljs-core-name)
            (assoc-in [:provide-index 'cljs.core] cljs-core-name)
            (assoc :compiled-core true)
            )))))

(defn step-finalize-config [state]
  (assoc state
         :configured true
         :main-deps {}
         :unoptimizable (when-let [imul (io/resource "cljs/imul.js")]
                          (slurp imul))
         ;; populate index with known sources
         :provide-index (into {} (for [{:keys [name provides]} (vals (:sources state))
                                       provide provides]
                                   [provide name]
                                   ))))

(defn- get-deps-for-ns* [state ns-sym]
  (let [name (get-in state [:provide-index ns-sym])]
    (when-not name
      (throw (ex-info "ns not available" {:ns ns-sym})))

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
                                          (add-goog-dependencies)
                                          (assoc :precompiled true)))
            (assoc-in [:provide-index ns] name)))
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

(defn find-sources-used-more-than-once
  [modules]
  (let [mi (reduce (fn [counts [src module]]
                     (update-in counts [src] set-conj module))
                   {}
                   (for [{:keys [name sources] :as mod} (vals modules)
                         src sources]
                     [src name]))]
    (->> mi
         (seq)
         (remove #(= (count (second %)) 1))
         (into {}))))

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
          module-set (set module-order)
          modules (reduce
                    (fn [modules module-name]
                      (let [{:keys [depends-on includes]} (get modules module-name)
                            parent-provides (reduce set/union (map #(set (get-in modules [% :sources])) depends-on))]
                        (update-in modules [module-name :sources] #(vec (remove parent-provides %)))))
                    modules
                    module-order)

          ;; move duplicate files in seperate modules that dont depend on each other
          ;; eg.
          ;; common
          ;; mod-a :require clojure.string :depends-on common
          ;; mod-b :require clojure.string :depends-on common
          ;; mod-a,mod-b would try to compile clojure/string.js blow up closure optimizer

          dups (find-sources-used-more-than-once modules)
          ;; bring back into dependency order
          sorted-dups (->> module-order
                           (mapcat #(get-in modules [% :sources]))
                           (distinct)
                           (filter #(contains? dups %))
                           (map (fn [dup-name]
                                  (let [dups (get dups dup-name)]
                                    [dup-name dups]
                                    ))))

          modules (loop [modules modules
                         dups sorted-dups]
                    (if-let [[source-name used-by] (first dups)]
                      (let [common-ancestor (first module-order) ;; FIXME: actually try to find one
                            _ (log-warning logger (format "Moving \"%s\" used by %s to %s" source-name used-by common-ancestor))
                            modules (reduce (fn [modules dep-mod]
                                              (update-in modules
                                                         [dep-mod :sources]
                                                         (fn [current]
                                                           (vec (remove #(= source-name %) current)))))
                                            modules
                                            used-by)
                            modules (update-in modules [common-ancestor :sources] conj source-name)]

                        (recur modules (rest dups)))
                      ;; done
                      modules
                      ))]

      (map #(get modules %) module-order)
      )))

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

(defn step-compile-modules [state]
  (with-logged-time
    [(:logger state) "Compiling Modules ..."]
    (let [state (reduce do-analyze-module state (-> state :modules (vals)))

          modules (sort-and-compact-modules state)
          source-names (->> modules
                            (mapcat :sources)
                            (map #(get-in state [:sources %]))
                            (filter #(= :cljs (:type %)))
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

(defn closure-defines-and-base [{:keys [public-path] :as state}]
  (let [goog-rc (get-in state [:sources goog-base-name])
        goog-base (:js-source goog-rc)]

    (when-not (seq goog-base)
      (throw (ex-info "no goog/base.js" {})))

    (when (< (count goog-base) 500)
      (throw (ex-info "probably not the goog/base.js you were expecting"
                      (get-in state [:sources goog-base-name]))))

    ;; TODO: defines should be actually defineable
    (str "var CLOSURE_NO_DEPS = true;\n"
         ;; goog.findBasePath_() requires a base.js which we dont have
         ;; this is usually only needed for unoptimized builds anyways
         "var CLOSURE_BASE_PATH = '" public-path "/src/';\n"
         "var CLOSURE_DEFINES = "
         (json/write-str (:closure-defines state {}))
         ";\n"
         goog-base
         "\n")))

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

                              (.add js-mod (SourceFile/fromCode js-name js-source)))

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
            js-source (str prepend js-source)
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
          cc (closure/make-closure-compiler)
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
         (let [{:keys [js-name requires provides] :as src} (get-in state [:sources src-name])]
           (str "goog.addDependency(\"" js-name "\","
                "[" (ns-list-string provides) "],"
                "[" (ns-list-string requires) "]);")))
       (str/join "\n")))

(defn flush-unoptimized
  [{:keys [build-modules public-dir public-path unoptimizable] :as state}]
  (when-not (seq build-modules)
    (throw (ex-info "flush before compile?" {})))
  (with-logged-time
    [(:logger state) "Flushing unoptimized modules"]

    (let [source-map? (boolean (:source-map state))]

      (doseq [{:keys [type name source] :as src} (->> (mapcat :sources build-modules)
                                                      (map #(get-in state [:sources %])))
              :let [target (io/file public-dir "src" name)]

              ;; skip files we already have since source maps are kinda expensive to generate
              :when (or (not (.exists target))
                        (> (or (:compiled-at src) ;; js is not compiled but maybe modified
                               (:last-modified src))
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
            )))

      (flush-manifest public-dir build-modules)

      ;; flush fake modules
      (doseq [{:keys [default js-name name prepend prepend-js append-js sources] :as mod} build-modules]
        (let [provided-ns (mapcat #(reverse (get-in state [:sources % :provides]))
                                  sources)
              target (io/file public-dir js-name)

              out (->> provided-ns
                       (map #(str "goog.require('" (comp/munge %) "');"))
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

          (spit target out)))))
  ;; return unmodified state
  state)

(defn get-reloadable-source-paths [state]
  (->> state
       :source-paths
       (vals)
       (filter :reloadable)
       (map :path)
       (set)))

(defn reset-resource [{:keys [name type ^File file] :as src} config]
  (-> src
      (dissoc :ns :ns-info :requires :provides :js-source :compiled :compiled-at)
      (read-resource config)
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
         (mapcat find-fs-resources)
         (remove (fn [{:keys [source-path name]}]
                   (contains? known-files [source-path name])))
         (map #(assoc % :scan :new))
         (into []))))

(defn scan-for-modified-files
  "scans known sources for modified or deleted files

  returns a seq of resource maps with a :scan key which is either :modified :delete"
  [{:keys [logger sources] :as state}]
  (let [reloadable-paths (get-reloadable-source-paths state)]
    (->> (vals sources)
         (filter :file)
         (filter #(contains? reloadable-paths (:source-path %)))
         (reduce (fn [result {:keys [name ^File file last-modified] :as rc}]
                   (cond
                     (not (.exists file))
                     (conj result (assoc rc :scan :delete))

                     (> (.lastModified file) last-modified)
                     (conj result (assoc rc :scan :modified))

                     :else
                     result))
                 []))))

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
  (reduce (fn [state {:keys [scan name file] :as rc}]
            (case scan
              :delete
              (do (log-progress logger (format "Found deleted file: %s -> %s" name file))
                  (unmerge-resource state (:name rc)))
              :new
              (do (log-progress logger (format "Found new file: %s -> %s" name file))
                  (merge-resources state [(-> rc (dissoc :scan) (reset-resource state))]))

              :modified
              (do (log-progress logger (format "File modified: %s -> %s" name file))
                  (merge-resources state [(-> rc (dissoc :scan) (reset-resource state))]))))
          state
          scan-results))

(defn wait-and-reload!
  "wait for modified files, reload them and return reloaded state"
  [{:keys [logger] :as state}]
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
      (assoc-in [:provide-index 'constants-table] "constants_table.js")))

(defn enable-source-maps [state]
  (assoc state :source-map "cljs.closure/make-options expects a string but we dont use it"))

(defn init-state []
  ;; static fns dont work in repl env, but i never used a cljs repl
  ;; need to look into cljs repl, otherwise I see no downside to using static fns
  (alter-var-root #'ana/*cljs-static-fns* (fn [_] true))

  {:compiler-env {} ;; will become env/*compiler*
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
