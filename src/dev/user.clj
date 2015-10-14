(ns user
  #_ (:import [java.io File])
  #_ (:require [clojure.java.io :as io]
            [clojure.java.javadoc :refer (javadoc)]
            [clojure.pprint :refer (pprint)]
            [clojure.reflect :refer (reflect)]
            [clojure.repl :refer (apropos dir doc find-doc pst source)]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.tools.namespace.track :as track]
            [clojure.tools.namespace.repl :as ns :refer (refresh refresh-all)]))

;; cmd+r workflow for the repl

(comment

  (ns/disable-reload!)

  (defonce globals (atom {}))

  (defmacro resume-from [& body]
    (let [key (pr-str body)]
      `(if-let [state# (get @globals ~key)]
         state#
         (let [state# (do ~@body)]
           (swap! globals assoc ~key state#)
           state#
           ))))

  (defn reset-globals! []
    (reset! globals {}))

  (in-ns 'clojure.tools.namespace.repl)

  ;; refresh-tracker is private ...
  (defn get-tracker []
    refresh-tracker)

  (in-ns 'user)

  (defn get-dependents-of-ns [ns-sym]
    (get-in (ns/get-tracker) [:clojure.tools.namespace.track/deps :dependents ns-sym]))

  (defn get-ns-of-file [^File file]
    (get-in (ns/get-tracker) [:clojure.tools.namespace.file/filemap (.getAbsoluteFile file)]))

  (defn is-test-ns? [mod-ns]
    (when mod-ns
      (re-find #"-test$" (name mod-ns))))

  (defn find-tests-using [mod-ns]
    (let [dependents (get-dependents-of-ns mod-ns)]
      (filter is-test-ns? dependents)))

  (defn expand-tests-to-run [^File file]
    (let [file (.getAbsoluteFile file)
          mod-ns (get-ns-of-file file)]
      (if (is-test-ns? mod-ns)
        (list mod-ns)
        (find-tests-using mod-ns)
        )))

  (defn run-tests [namespaces tags]
    (try
      (let [start (System/currentTimeMillis)]
        (loop [namespaces namespaces
               total-count {}]
          (if (empty? namespaces)
            (do
              (test/do-report (assoc total-count :type :summary))
              total-count)
            (recur (rest namespaces)
                   (binding [test/*report-counters* (ref test/*initial-report-counters*)]
                     (let [ns (the-ns (first namespaces))]
                       (test/do-report {:type :begin-test-ns, :ns ns})
                       (let [once-fixture-fn (test/join-fixtures (:clojure.test/once-fixtures (meta ns)))
                             each-fixture-fn (test/join-fixtures (:clojure.test/each-fixtures (meta ns)))]
                         (once-fixture-fn
                           (fn []
                             (doseq [v (vals (ns-interns ns))]
                               (let [vm (meta v)]
                                 (when (and (:test vm)
                                            (some #(get vm %) tags))
                                   (each-fixture-fn (fn []
                                                      (println "- " (:name vm))
                                                      (test/test-var v)))))))))
                       (test/do-report {:type :end-test-ns, :ns ns}))
                     (merge-with + total-count @test/*report-counters*)))))

        (println (str "Runtime: " (- (System/currentTimeMillis) start) "ms"))
        (.flush System/out))
      ))


  ;; combine with emacs

  (comment
    (defun cider-run-tests ()
           (interactive)
           (cider-find-and-clear-repl-buffer)
           (cider-eval (concat "(run-applicable-tests [\"" buffer-file-name "\"])")
                       (cider-interactive-eval-handler (current-buffer))
                       "user"))
    (global-set-key (kbd "s-r") 'cider-run-tests))

  (defn run-applicable-tests [modified-files]
    (refresh)
    (let [tests-to-run (->> modified-files
                            (map io/file)
                            (mapcat expand-tests-to-run))]
      (if (empty? tests-to-run)
        (do
          (println "================================================================================")
          (println "WARNING: There are no tests for:" modified-files)
          (println "================================================================================"))
        (do
          (println "================================================================================")
          (run-tests tests-to-run #{:wip})
          (println "================================================================================"))
        ))))
