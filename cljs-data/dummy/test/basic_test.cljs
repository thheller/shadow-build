(ns basic-test
  (:require-macros [cljs.test :refer (deftest)])
  (:require [basic]
            [cljs.test :as test]))

(deftest test-yo
  (prn :foo/bar))


