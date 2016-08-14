(ns shadow.cljs.node
  (:refer-clojure :exclude [flush compile])
  (:require [shadow.cljs.build :as cljs]
            [clojure.java.io :as io]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str]
            [cljs.compiler :as comp]
            [clojure.data.json :as json])
  (:import (java.io File)))

;; FIXME: this is very very ugly, probably breaks easily
(defn make-main-call-js [main-fn]
  (str/join
    "\n"
    ["(function() {"
     "var proc = require('process');"
     "cljs.core.apply.cljs$core$IFn$_invoke$arity$2(" (comp/munge main-fn) ","
     "process.argv.slice(2)"
     ");"
     "})();"]
    ))

(defn configure
  [state {:keys [main output-to] :as opts}]
  (let [main-ns (namespace main)
        [main-ns main-fn] (if (nil? main-ns)
                            [(name main) "main"]
                            [main-ns (name main)])
        output-to (io/file output-to)
        output-name (.getName output-to)
        module-name (-> output-name (str/replace #".js$" "") (keyword))
        output-dir (.getParentFile output-to)

        main (symbol main-ns main-fn)]

    (-> state
        (assoc :public-dir output-dir)
        (cljs/reset-modules)
        (cljs/configure-module module-name [(symbol main-ns)] #{} {:append-js (make-main-call-js main)})
        )))

(defn compile [state]
  (cljs/compile-modules state))

(defn optimize [state]
  (cljs/closure-optimize state))


(defn replace-goog-global [s node-global-prefix]
  (str/replace s
    ;; browsers have window as this
    #"goog.global(\s?)=(\s?)this;"
    ;; node "this" is the local module, global is the actual global
    (str "goog.global=" node-global-prefix ";")))

(defn closure-defines-and-base
  "basically the same as cljs/closure-defines-and-base except that is sets the defines in global as well
   also assumes that the file containing this code is the root we can use to lookup paths"
  [{:keys [node-global-prefix] :as state}]
  (let [goog-rc (get-in state [:sources cljs/goog-base-name])
        goog-base @(:input goog-rc)]

    (when-not (seq goog-base)
      (throw (ex-info "no goog/base.js" {})))

    ;; FIXME: work arround for older cljs versions that used broked closure release, remove.
    (when (< (count goog-base) 500)
      (throw (ex-info "probably not the goog/base.js you were expecting"
               (get-in state [:sources cljs/goog-base-name]))))

    (str "\nvar CLOSURE_NO_DEPS = " node-global-prefix ".CLOSURE_NO_DEPS = true;\n"
         ;; FIXME: this still has hardcoded cljs-runtime-path
         (-> (io/resource "shadow/cljs/infer_closure_base_path.js")
             (slurp)
             (cond->
               (not= node-global-prefix "global")
               (str/replace "global." (str node-global-prefix "."))))
         "\nvar CLOSURE_DEFINES = " node-global-prefix ".CLOSURE_DEFINES = "
         (json/write-str (:closure-defines state {}))
         ";\n"
         goog-base
         "\n")))

(defn flush-unoptimized
  [{:keys [build-modules public-dir cljs-runtime-path node-global-prefix] :as state}]
  {:pre [(cljs/directory? public-dir)]}
  (when (not= 1 (count build-modules))
    (throw (ex-info "node builds can only have one module!" {})))

  ;; FIXME: does node need the imul.js fix? probably not
  (cljs/flush-sources-by-name state (mapcat :sources build-modules))

  (let [output-file
        (-> build-modules first :js-name)]

    (cljs/with-logged-time
      [state {:type ::flush-unoptimized
              :output-file output-file}]

      (let [{:keys [js-name prepend prepend-js append-js sources]} (first build-modules)]
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
              goog-js (io/file public-dir cljs-runtime-path "goog" "base.js")
              deps-js (io/file public-dir cljs-runtime-path "deps.js")]
          (spit goog-js
            (replace-goog-global
              @(get-in state [:sources "goog/base.js" :input])
              node-global-prefix
              ))
          (spit deps-js (cljs/closure-goog-deps state))
          (spit target out)))))
  ;; return unmodified state
  state)


(defn flush-optimized
  [{modules :optimized :keys [^File public-dir logger node-global-prefix] :as state}]
  (cljs/with-logged-time
    [state {:type ::flush-unoptimized}]

    (when (not= 1 (count modules))
      (throw (ex-info "node builds can only have one module!" {})))

    (when-not (seq modules)
      (throw (ex-info "flush before optimize?" {})))

    (when-not public-dir
      (throw (ex-info "missing :public-dir" {})))

    (let [{:keys [output prepend append source-map-name name js-name] :as mod} (first modules)
          target (io/file public-dir js-name)

          out (str prepend
                   (cljs/foreign-js-source-for-mod state mod)
                   (replace-goog-global output node-global-prefix)
                   append)]

      (io/make-parents target)
      (spit target out)

      (cljs/log state {:type :flush-module
                       :js-name js-name
                       :js-size (count out)})

      ;; FIXME: add source-map support for node
      #_(when source-map-name
          (spit target (str "\n//# sourceMappingURL=src/" (cljs/file-basename source-map-name) "\n")
            :append true))
      ))

  #_(when (:source-map state)
      (flush-source-maps state))

  state)

(defn flush [{:keys [optimizations] :as state}]
  (if (= optimizations :none)
    (flush-unoptimized state)
    (flush-optimized state)))

(defn execute! [{:keys [logger public-path] :as state} program & args]
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
  (let [require-order (into ['cljs.core 'runtime-setup 'cljs.test] test-namespaces)
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

