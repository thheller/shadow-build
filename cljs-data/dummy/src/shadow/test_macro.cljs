(ns shadow.test-macro
  (:require-macros [shadow.test-macro]))

(defn hello [& something]
  :NORMAL)

(defn not-a-macro [] :yo)