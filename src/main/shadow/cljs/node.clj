(ns shadow.cljs.node
  (:refer-clojure :exclude [flush compile])
  (:require [shadow.cljs.build :as cljs]
            [shadow.cljs.log :as log]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [cljs.compiler :as comp]
            [clojure.data.json :as json])
  (:import (java.lang ProcessBuilder$Redirect)))

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
          (io/file "target" "shadow-node" main-ns))

        main
        (symbol main-ns main-fn)

        node-config
        (assoc opts :main-ns main-ns
          :main-fn main-fn
          :main main
          :output-to output-to
          :public-dir public-dir)

        main-call
        (-> node-config :main (make-main-call-js))

        module-opts
        (-> opts
            (select-keys [:prepend :append :prepend-js :append-js])
            (update :append-js str "\n" main-call))]

    (-> state
        (assoc :node-config node-config)
        (assoc :public-dir public-dir)
        (cljs/reset-modules)
        (cljs/configure-module module-name [(symbol main-ns)] #{} module-opts)
        )))

(defn compile [state]
  (cljs/compile-modules state))

(defn optimize [state]
  (cljs/closure-optimize state (get-in state [:node-opts :optimization] :simple)))

(defn closure-defines
  [state]
  (str "\nSHADOW_ENV.CLOSURE_NO_DEPS = true;\n"
       "\nSHADOW_ENV.CLOSURE_DEFINES = " (json/write-str (:closure-defines state {})) ";\n"))

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

  (cljs/flush-sources-by-name state)

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

               ;; this is here and not in boostrap since defines already accesses them
               (str "var SHADOW_IMPORT_PATH = \""
                    (-> (io/file public-dir cljs-runtime-path)
                        (.getAbsolutePath))
                    "\";")
               (str "var SHADOW_ENV = {};")

               (when source-map
                 (str "try {"
                      "require('source-map-support').install();"
                      "} catch (e) {"
                      "console.warn('no \"source-map-support\" (run \"npm install source-map-support --save-dev\" to get it)');"
                      "}"))

               ;; FIXME: these operate on SHADOW_ENV
               ;; this means they rely on goog.global = this AND fn.call(SHADOW_ENV, ...)
               ;; I eventually want to turn the "this" of shadow imports into the module
               ;; to match what node does.
               (closure-defines state)

               ;; provides SHADOW_IMPORT and other things
               (slurp (io/resource "shadow/cljs/node_bootstrap.txt"))

               ;; manually import goog/base.js so we can patch it before others get imported
               "SHADOW_IMPORT(\"goog/base.js\");"
               "goog.provide = SHADOW_PROVIDE;"
               "goog.require = SHADOW_REQUIRE;"

               ;; import all other sources
               (->> sources
                    (remove #{"goog/base.js"})
                    (map #(get-in state [:sources %]))
                    (map :js-name)
                    (map (fn [src]
                           (str "SHADOW_IMPORT(" (pr-str src) ");")))
                    (str/join "\n"))

               ;; make these local always
               ;; these are needed by node/configure :main and the umd exports
               "var shadow = SHADOW_ENV.shadow || {};"
               "var cljs = SHADOW_ENV.cljs || {};"

               (when-some [main (:main node-config)]
                 (let [root
                       (-> (str main)
                           (comp/munge))
                       root
                       (subs root 0 (str/index-of root "."))
                       ]
                   (str "var " root " = SHADOW_ENV." root ";")))

               append-js
               append])]

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

(defmethod log/event->str ::execute!
  [{:keys [args]}]
  (format "Execute: %s" (pr-str args)))

(defn execute! [{:keys [node-config] :as state}]
  (when (not= 1 (-> state :build-modules count))
    (throw (ex-info "can only execute non modular builds" {})))

  (let [{:keys [output-to]}
        node-config

        script-args
        ["node"]

        pb
        (doto (ProcessBuilder. script-args)
          (.directory nil)
          ;; (.directory public-dir)
          (.redirectOutput ProcessBuilder$Redirect/INHERIT)
          (.redirectError ProcessBuilder$Redirect/INHERIT))]

    ;; not using this because we only get output once it is done
    ;; I prefer to see progress
    ;; (prn (apply shell/sh script-args))

    (cljs/with-logged-time
      [state {:type ::execute!
              :args script-args}]
      (let [proc
            (.start pb)]

        (let [out (.getOutputStream proc)]
          (io/copy (io/file output-to) out)
          (.close out))

        ;; FIXME: what if this doesn't terminate?
        (let [exit-code (.waitFor proc)]
          (assoc state ::exit-code exit-code))))))

(defn setup-test-runner [state test-namespaces]
  (let [require-order
        (into ['cljs.core 'shadow.runtime-setup 'cljs.test] test-namespaces)

        test-runner-ns
        'shadow.test-runner

        test-runner-src
        {:name "shadow/test_runner.cljs"
         :js-name "shadow/test_runner.js"
         :type :cljs
         :ns test-runner-ns
         :provides #{test-runner-ns}
         :requires (into #{} require-order)
         :require-order require-order
         :input (atom [`(~'ns ~test-runner-ns
                          (:require [cljs.test]
                            ~@(mapv vector test-namespaces)))

                       `(defmethod cljs.test/report [:cljs.test/default :end-run-tests] [m#]
                          (if (cljs.test/successful? m#)
                            (js/process.exit 0)
                            (js/process.exit 1)
                            ))

                       `(defn ~'main []
                          (cljs.test/run-tests
                            (cljs.test/empty-env)
                            ~@(for [it test-namespaces]
                                `(quote ~it))))])
         :last-modified (System/currentTimeMillis)}]

    (-> state
        (cljs/merge-resource test-runner-src)
        (cljs/reset-modules)
        (configure {:main test-runner-ns :output-to "target/shadow-test-runner.js"}))))

(defn find-all-test-namespaces [state]
  (->> (get-in state [:sources])
       (vals)
       (remove :jar)
       (filter cljs/has-tests?)
       (map :ns)
       (remove #{'shadow.test-runner})
       (into [])))

(defn make-test-runner
  ([state]
   (make-test-runner state (find-all-test-namespaces state)))
  ([state test-namespaces]
   (-> state
       (setup-test-runner test-namespaces)
       (compile)
       (flush-unoptimized))))

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
  (let [source-names
        (->> source-names
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
              (execute!))
          ;; return unmodified state, otherwise previous module information and config is lost
          state))))

(defn execute-all-tests! [state]
  (-> state
      (make-test-runner)
      (execute!))

  ;; return unmodified state!
  state
  )

(defn execute-all-tests-and-exit! [state]
  (let [state (-> state
                  (make-test-runner)
                  (execute!))]
    (System/exit (::exit-code state))))

