(ns shadow.test-macro)

(defmacro hello [& anything]
  `(prn "world") )
