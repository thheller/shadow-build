(ns shadow.dummy
  (:require [shadow.test-macro :as tm :refer (a-macro not-a-macro)]))

(tm/hello "world")
