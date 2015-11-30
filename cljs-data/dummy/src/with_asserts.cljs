(ns with-asserts)

(defn foo? [arg] (= "foo" arg))

(defn got-some-asserts [arg]
  {:pre [(string? arg)
         foo?]
   :post [(foo? %)]}
  arg)


(js/console.log (got-some-asserts "bar"))