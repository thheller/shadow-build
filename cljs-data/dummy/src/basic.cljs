(ns basic
  (:require [shadow.test-macro :as tm]
            [clojure.string :as str]
            [common :as c]))

(defn ^:export hello []
  (.log js/console (str/join " " [:hello :world ::test])))

(defn ^:export start-app []
  (tm/hello)
  (assert (keyword-identical? ::test :basic/test))
  (.log js/console "start app"))


(defn ^:export stop-app []
  (.log js/console "stop app"))

(.log js/console "finished loading basic" (c/test-it ::c/test))

(prn [:look "print"])