(ns shadow.cljs.node
  (:refer-clojure :exclude [flush compile])
  (:require [shadow.cljs.build :as cljs]
            [shadow.cljs.log :as log]
            [clojure.java.io :as io]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str]
            [cljs.compiler :as comp]
            [clojure.data.json :as json]))

(defmethod log/event->str ::flush-unoptimized
  [{:keys [output-file] :as ev}]
  (str "Flush node script: " output-file))

(defmethod log/event->str ::flush-optimized
  [{:keys [output-file] :as ev}]
  (str "Flush optimized node script: " output-file))

(defn make-main-call-js [main-fn]
  {:pre [(symbol? main-fn)]}
  (str "\ncljs.core.apply.cljs$core$IFn$_invoke$arity$2(" (comp/munge main-fn) ", process.argv.slice(2));"))

(defn configure
  [state {:keys [main public-dir output-to] :as opts}]
  (let [main-ns
        (namespace main)

        [main-ns main-fn]
        (if (nil? main-ns)
          [(name main) "main"]
          [main-ns (name main)])

        output-to
        (io/file output-to)

        output-name
        (.getName output-to)

        module-name
        (-> output-name (str/replace #".js$" "") (keyword))

        public-dir
        (if (seq public-dir)
          (io/file public-dir)
          (io/file "target" "shadow.node" main-ns))

        main
        (symbol main-ns main-fn)

        node-config
        (assoc opts :main-ns main-ns
                    :main-fn main-fn
                    :main main
                    :output-to output-to
                    :public-dir public-dir)]

    (-> state
        (assoc :node-config node-config)
        (assoc :public-dir public-dir)
        (cljs/reset-modules)
        (cljs/configure-module module-name [(symbol main-ns)] #{} {:append-js (-> node-config :main (make-main-call-js))})
        )))

(defn compile [state]
  (cljs/compile-modules state))

(defn optimize [state]
  (cljs/closure-optimize state (get-in state [:node-opts :optimization] :simple)))

(defn closure-defines
  [state]
  (str "\nCLOSURE_GLOBAL.CLOSURE_NO_DEPS = true;\n"
       "\nCLOSURE_GLOBAL.CLOSURE_DEFINES = " (json/write-str (:closure-defines state {})) ";\n"))

(defn replace-goog-global [s]
  (str/replace s
    ;; browsers have window as this
    #"goog.global(\s?)=(\s?)this;"
    ;; node "this" is the local module, global is the actual global
    (str "goog.global=global;")))

(defn closure-base
  [state]
  (let [goog-rc (get-in state [:sources cljs/goog-base-name])]
    @(:input goog-rc)
    ))

(defn flush-unoptimized
  [{:keys [build-modules cljs-runtime-path source-map public-dir node-config] :as state}]
  {:pre [(cljs/directory? public-dir)]}
  (when (not= 1 (count build-modules))
    (throw (ex-info "node builds can only have one module!" {})))

  (cljs/flush-sources-by-name state (mapcat :sources build-modules))

  (let [{:keys [output-to]}
        node-config]

    (cljs/with-logged-time
      [state {:type ::flush-unoptimized
              :output-file (.getAbsolutePath output-to)}]

      (let [{:keys [prepend prepend-js append-js append sources]}
            (first build-modules)

            out
            (str/join "\n"
              [prepend
               prepend-js

               (str "var CLOSURE_GLOBAL = {};")

               (when source-map
                 (str "try {"
                      "require('source-map-support').install();"
                      "} catch (e) {"
                      "console.warn('no source map support, install npm source-map-support');"
                      "}"))

               (closure-defines state)

               (str "var CLOSURE_IMPORT_PATH = \""
                    (-> (io/file public-dir cljs-runtime-path)
                        (.getAbsolutePath))
                    "\";")

               ;; provides CLOSURE_IMPORT_SCRIPT and other things
               (slurp (io/resource "shadow/cljs/node_bootstrap.txt"))

               "CLOSURE_IMPORT_SCRIPT(\"goog/base.js\");"

               "goog.provide = CLOSURE_PROVIDE;"
               "goog.require = CLOSURE_REQUIRE;"

               (->> sources
                    (map #(get-in state [:sources %]))
                    (map :js-name)
                    (map (fn [src]
                           (str "CLOSURE_IMPORT_SCRIPT(" (pr-str src) ");")))
                    (str/join "\n"))

               ;; make these local always
               ;; these are needed by node/configure :main and the umd exports
               "var shadow = CLOSURE_GLOBAL.shadow || {};"
               "var cljs = CLOSURE_GLOBAL.cljs || {};"

               append-js
               append])]

        (let [base-js
              (io/file public-dir cljs-runtime-path "goog" "base.js")]
          (spit base-js (closure-base state)))

        (io/make-parents output-to)
        (spit output-to out))))

  ;; return unmodified state
  state)


(defn flush-optimized
  [{modules :optimized :keys [node-config] :as state}]
  (let [{:keys [output-to]} node-config]
    (cljs/with-logged-time
      [state {:type ::flush-optimized
              :output-file (.getAbsolutePath output-to)}]

      (when (not= 1 (count modules))
        (throw (ex-info "node builds can only have one module!" {})))

      (when-not (seq modules)
        (throw (ex-info "flush before optimize?" {})))

      (let [{:keys [output prepend append js-name]}
            (first modules)

            ;; prepend-js and append-js went through the closure compiler
            out
            (str prepend output append)]

        (io/make-parents output-to)
        (spit output-to out)

        ;; FIXME: add source-map support for node
        #_(when source-map-name
            (spit target (str "\n//# sourceMappingURL=src/" (cljs/file-basename source-map-name) "\n")
              :append true))
        )))


  state)

(defn flush [{:keys [optimizations] :as state}]
  (if (= optimizations :none)
    (flush-unoptimized state)
    (flush-optimized state)))

(defn execute! [{:keys [public-path] :as state} program & args]
  (when (not= 1 (-> state :build-modules count))
    (throw (ex-info "can only execute non modular builds" {})))

  (let [script-name
        (-> state :build-modules first :js-name)

        script-args
        (->> args
             (map (fn [arg]
                    (cond
                      (string? arg)
                      arg
                      (= :script arg)
                      (str public-path "/" script-name)
                      :else
                      (throw (ex-info "invalid execute args" {:args args})))))
             (into [program]))

        pb
        (doto (ProcessBuilder. script-args)
          ;; (.directory public-dir)
          (.inheritIO))]

    ;; not using this because we only get output once it is done
    ;; I prefer to see progress
    ;; (prn (apply shell/sh script-args))

    (cljs/with-logged-time
      [state {:type ::execute!
              :args script-args}]
      (let [proc (.start pb)
            ;; FIXME: what if this doesn't terminate?
            exit-code (.waitFor proc)]
        (assoc state ::exit-code exit-code)))))

(defn setup-test-runner [state test-namespaces]
  (let [require-order
        (into ['cljs.core 'shadow.runtime-setup 'cljs.test] test-namespaces)
        test-runner-src
        {:name "test_runner.cljs"
         :js-name "test_runner.js"
         :type :cljs
         :provides #{'test-runner}
         :requires (into #{} require-order)
         :require-order require-order
         :ns 'test-runner
         :input (atom [`(~'ns ~'test-runner
                          (:require [cljs.test]
                            ~@(mapv vector test-namespaces)))

                       `(defmethod cljs.test/report [:cljs.test/default :end-run-tests] [m#]
                          (if (cljs.test/successful? m#)
                            (js/process.exit 0)
                            (js/process.exit 1)
                            ))

                       `(cljs.test/run-tests
                          (cljs.test/empty-env)
                          ~@(for [it test-namespaces]
                              `(quote ~it)))])
         :last-modified (System/currentTimeMillis)}]

    (-> state
        (cljs/merge-resource test-runner-src)
        (cljs/reset-modules)
        (cljs/configure-module :test-runner ['test-runner] #{}))))

(defn find-all-test-namespaces [state]
  (->> (get-in state [:sources])
       (vals)
       (remove :jar)
       (filter cljs/has-tests?)
       (map :ns)
       (into [])))

(defn make-test-runner
  ([state]
   (make-test-runner state (find-all-test-namespaces state)))
  ([state test-namespaces]
   (-> state
       (setup-test-runner test-namespaces)
       (cljs/compile-modules)
       (flush))))


(defn to-source-name [state source-name]
  (cond
    (string? source-name)
    source-name
    (symbol? source-name)
    (get-in state [:provide->source source-name])
    :else
    (throw (ex-info (format "no source for %s" source-name) {:source-name source-name}))
    ))

(defn execute-affected-tests!
  [state source-names]
  (let [source-names (->> source-names
                          (map #(to-source-name state %))
                          (into []))
        test-namespaces
        (->> (concat source-names (cljs/find-dependents-for-names state source-names))
             (filter #(cljs/has-tests? (get-in state [:sources %])))
             (map #(get-in state [:sources % :ns]))
             (distinct)
             (into []))]

    (if (empty? test-namespaces)
      (do (cljs/log state {:type :info
                           :msg (format "No tests to run for: %s" (pr-str source-names))})
          state)
      (do (-> state
              (make-test-runner test-namespaces)
              (execute! "node" :script))
          ;; return unmodified state, otherwise previous module information and config is lost
          state))))

(defn execute-all-tests! [state]
  (-> state
      (make-test-runner)
      (execute! "node" :script))

  ;; return unmodified state!
  state
  )

(defn execute-all-tests-and-exit! [state]
  (let [state (-> state
                  (make-test-runner)
                  (execute! "node" :script))]
    (System/exit (::exit-code state))))

