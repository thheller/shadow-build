(ns shadow.test-macro
  {:load-macros true})

(defn hello [& something]
  :NORMAL)
