(ns with-rename
  (:refer-clojure :rename {assoc cossa + plus})
  (:require [clojure.string :refer (starts-with?) :rename {starts-with? foo}]
            [cljs.test :refer (deftest) :rename {deftest bar}]))

(foo "x" "y")
(cossa {} :x 1)

(plus 1 2)

(cljs.core/+ 1 2)

(apply plus [1 2])
(apply cljs.core/+ [1 2])

(bar test-rename-a-macro
  :yo)

