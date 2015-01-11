(ns common)

(prn [:look "i can print"])

(defn test-it [x]
  (.log js/console "test" (= x ::test) (pr-str (vals {:a "a"})))
  (= x ::test))

(test-it ::test-it)
