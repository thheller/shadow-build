(ns basic
  (:require [clojure.string :as str]
            [common :as c]))

(defn ^:export hello []
  (.log js/console (str/join " " [:hello :world ::test])))

(assert (keyword-identical? ::test :basic/test))

(.log js/console "finished loading basic" (c/test ::c/test))

