(ns shadow.fix-test)

(in-ns 'clojure.test)

(defmethod report :error [m]
  (with-test-out
    (inc-report-counter :error)
    (println "\nERROR in" (testing-vars-str m))
    (when (seq *testing-contexts*) (println (testing-contexts-str)))
    (when-let [message (:message m)] (println message))
    (println "expected:" (pr-str (:expected m)))
    (let [actual (:actual m)]
      (if (instance? Throwable actual)
        (do
          (when (instance? clojure.lang.ExceptionInfo actual)
            (doseq [[k v] (->> actual ex-data (sort-by first))]
              (println (str "ex-data[" k "]: " (pr-str v)))))
          (print "  actual: ")
          (stack/print-cause-trace actual *stack-trace-depth*))
        (do
          (print "  actual: ")
          (prn actual))))))

;; (in-ns 'shadow.fix-test)
