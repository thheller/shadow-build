(ns shadow.test-macro)

(defmacro hello [& anything]
  `(prn "world") )

(defmacro a-macro [& anything]
  `(prn "a-macro"))
