(ns basic
  (:require [clojure.string :as str]
            [common :as c]))

(defn ^:export hello []
  (.log js/console (str/join " " [:hello :world ::test])))

(assert (keyword-identical? ::test :basic/test))

(defn ^:export start-app []
  (.log js/console "start app"))

(defn ^:export stop-app []
  (.log js/console "stop app"))

(.log js/console "finished loading basic" (c/test ::c/test))

