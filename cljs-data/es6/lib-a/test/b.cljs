(ns test.b
  (:require [test.a :as a]))

#_ (es6-import a :from "./a")

(defn b [x]
  x)

(js/console.log (a/foo {}))