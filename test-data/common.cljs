(ns common)

(defn test [x]
  (.log js/console "test" (= x ::test) (pr-str (vals {:a "a"})))
  (= x ::test))

(test ::test)
