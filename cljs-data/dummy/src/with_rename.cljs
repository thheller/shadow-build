(ns with-rename
  (:refer-clojure :rename {assoc cossa})
  (:require [clojure.string :refer (starts-with?) :rename {starts-with? foo}]))

(foo "x" "y")
(cossa {} :x 1)