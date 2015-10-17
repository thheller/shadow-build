(ns test.server
  (:require [clojure.string :as str]))

(prn "yo")

(defn main [& args]
  (prn [:main (map str/upper-case args)]))


(prn :wtf)

