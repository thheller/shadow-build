(ns shadow.cljs.node
  (:refer-clojure :exclude [flush compile])
  (:require [shadow.cljs.build :as cljs]
            [clojure.java.io :as io]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str]
            [cljs.compiler :as comp])
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


(defn replace-goog-global [s]
  (str/replace s
    ;; browsers have window as this
    #"goog.global = this;"
    ;; node "this" is the local module, global is the actual global
    "goog.global = global;"))

(defn flush-unoptimized
  [{:keys [build-modules public-dir] :as state}]
  {:pre [(cljs/directory? public-dir)]}
  (when (not= 1 (count build-modules))
    (throw (ex-info "node builds can only have one module!" {})))

  ;; FIXME: does node need the imul.js fix? probably not

  (cljs/flush-sources-by-name state (mapcat :sources build-modules))

  (cljs/with-logged-time
    [(:logger state) (format "Flushing node script: %s" (-> build-modules first :js-name))]

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
                     "\n"
                     "goog.global = global;"
                     "\n\n"
                     out)
            goog-js (io/file public-dir "src" "goog" "base.js")
            deps-js (io/file public-dir "src" "deps.js")]
        (spit goog-js @(get-in state [:sources "goog/base.js" :input]))
        (spit deps-js (cljs/closure-goog-deps state))
        (spit target out))))
  ;; return unmodified state
  state)


(defn flush-optimized
  [{modules :optimized :keys [unoptimizable ^File public-dir public-path logger] :as state}]
  (cljs/with-logged-time
    [(:logger state) "Flushing to disk"]

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
                   (replace-goog-global output)
                   append)]

      (io/make-parents target)
      (spit target out)

      (cljs/log-progress logger (format "Wrote module \"%s\" (size: %d)" js-name (count out)))

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

    (cljs/with-logged-time
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
                         :requires (into #{'cljs.core 'cljs.test} test-namespaces)
                         :ns 'test-runner
                         :input (atom [`(~'ns ~'test-runner
                                          (:require [cljs.test]
                                            ~@(mapv vector test-namespaces)))
                                       `(cljs.test/run-tests (cljs.test/empty-env)
                                          ~@(for [it test-namespaces]
                                              `(quote ~it)))])
                         :last-modified (System/currentTimeMillis)}]

    (-> state
        (cljs/merge-resource test-runner-src)
        (cljs/reset-modules)
        (cljs/configure-module :test-runner ['test-runner] #{}))))

(defn make-test-runner [state test-namespaces]
  (-> state
      (setup-test-runner test-namespaces)
      (cljs/compile-modules)
      (flush)))

(defn execute-affected-tests!
  [{:keys [logger] :as state} source-names]
  (let [test-namespaces
        (->> (concat source-names (cljs/find-dependents-for-names state source-names))
             (filter #(cljs/has-tests? (get-in state [:sources %])))
             (map #(get-in state [:sources % :ns]))
             (distinct)
             (into []))]

    (if (empty? test-namespaces)
      (do (cljs/log-progress logger (format "No tests to run for: %s" (pr-str source-names)))
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
                             (filter cljs/has-tests?)
                             (map :ns)
                             (into []))]
    (-> state
        (make-test-runner test-namespaces)
        (execute! "node" :script))

    ;; return unmodified state!
    state
    ))

