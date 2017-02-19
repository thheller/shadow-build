(ns shadow.cljs.node
  (:refer-clojure :exclude [flush compile])
  (:require [shadow.cljs.build :as cljs]
            [shadow.cljs.log :as log]
            [clojure.java.io :as io]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str]
            [cljs.compiler :as comp]
            [clojure.data.json :as json])
  (:import (java.io File)))


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

(defn replace-goog-global [s node-global-prefix]
  (str/replace s
    ;; browsers have window as this
    #"goog.global(\s?)=(\s?)this;"
    ;; node "this" is the local module, global is the actual global
    (str "goog.global=" node-global-prefix ";")))


(defn closure-defines
  [{:keys [node-global-prefix] :as state}]
  (str "\nvar CLOSURE_NO_DEPS = " node-global-prefix ".CLOSURE_NO_DEPS = true;\n"
       "\nvar CLOSURE_DEFINES = " node-global-prefix ".CLOSURE_DEFINES = "
       (json/write-str (:closure-defines state {}))
       ";\n"))

(defn closure-base
  [state]
  (let [goog-rc (get-in state [:sources cljs/goog-base-name])]
    @(:input goog-rc)
    ))

(defmethod log/event->str ::flush-unoptimized
  [{:keys [output-file] :as ev}]
  (str "Flush node script: " output-file))

(defmethod log/event->str ::flush-optimized
  [{:keys [output-file] :as ev}]
  (str "Flush optimized node script: " output-file))


(defn flush-unoptimized
  [{:keys [build-modules cljs-runtime-path source-map public-dir node-global-prefix node-config] :as state}]
  {:pre [(cljs/directory? public-dir)]}
  (when (not= 1 (count build-modules))
    (throw (ex-info "node builds can only have one module!" {})))

  (cljs/flush-sources-by-name state (mapcat :sources build-modules))

  (let [{:keys [output-to]}
        node-config]

    (cljs/with-logged-time
      [state {:type ::flush-unoptimized
              :output-file (.getAbsolutePath output-to)}]

      (let [{:keys [js-name prepend prepend-js append-js sources]}
            (first build-modules)

            out
            (str/join "\n"
              [;; FIXME: what if there are two?
               ;; we would break the other CLOSURE_IMPORT_SCRIPT and probably others things by replacing them
               "if (global.goog) { throw new Error('cannot have two GOOG'); }"

               prepend
               prepend-js

               (when source-map
                 (str "try {"
                      "require('source-map-support').install();"
                      "} catch (e) {"
                      "console.warn('no source map support, install npm source-map-support');"
                      "}"))

               (closure-defines state)

               (str "var NODE_INCLUDE_PATH = \""
                    (-> (io/file public-dir cljs-runtime-path)
                        (.getAbsolutePath))
                    "\";")

               ;; provides CLOSURE_IMPORT_SCRIPT and other things
               (slurp (io/resource "shadow/cljs/node_bootstrap.txt"))

               ;; inlining goog/base.js
               #_(replace-goog-global
                   (closure-base state)
                   node-global-prefix)

               ;; inline goog/deps.js (CLOSURE_NO_DEPS set by closure-defines)
               ;; bypassing loading things per goog.require
               #_(cljs/closure-goog-deps state sources)

               #_(->> sources
                      (mapcat #(reverse (get-in state [:sources %])))
                      (map (fn [ns]
                             (str "goog.require('" (comp/munge ns) "');")))
                      (str/join "\n"))

               "CLOSURE_IMPORT_SCRIPT(\"goog/base.js\");"

               ;; FIXME: only need this when running a REPL with hot loading
               "goog.isProvided_ = function(name) { return false; }"

               ;; FIXME: good idea to noop require?
               "goog.require = function(name) { return true; }"

               (->> sources
                    (map #(get-in state [:sources %]))
                    (map :js-name)
                    (map (fn [src]
                           (str "CLOSURE_IMPORT_SCRIPT(" (pr-str src) ");")))
                    (str/join "\n"))
               append-js])]

        (let [base-js
              (io/file public-dir cljs-runtime-path "goog" "base.js")]

          (spit base-js
            (replace-goog-global
              (closure-base state)
              node-global-prefix)))

        (io/make-parents output-to)
        (spit output-to out))))

  ;; return unmodified state
  state)


(defn flush-optimized
  [{modules :optimized :keys [node-global-prefix node-config] :as state}]
  (let [{:keys [output-to]} node-config]
    (cljs/with-logged-time
      [state {:type ::flush-optimized
              :output-file (.getAbsolutePath output-to)}]

      (when (not= 1 (count modules))
        (throw (ex-info "node builds can only have one module!" {})))

      (when-not (seq modules)
        (throw (ex-info "flush before optimize?" {})))

      (let [{:keys [output prepend js-name]}
            (first modules)

            out
            (str prepend
                 (replace-goog-global output node-global-prefix))]

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
  [{:keys [logger] :as state} source-names]
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

