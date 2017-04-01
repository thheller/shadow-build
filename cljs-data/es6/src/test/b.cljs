(ns test.b
  (:require [module$test$a :as a]))

(defn b [x]
  x)

(js/console.log (a/foo {}))