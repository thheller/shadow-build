(ns shadow.cljs.passes)

(defn macro-js-requires [env {:keys [op] :as ast}]
  (if (and (= :ns op) (:use-macros ast))
    (let [requires (reduce (fn [requires [macro-name macro-ns]]
                             (let [{:keys [js-require] :as m} (-> (symbol (str macro-ns) (str macro-name))
                                                            (find-var)
                                                            (meta))]
                               (cond
                                (symbol? js-require)
                                (assoc requires js-require js-require)

                                (coll? js-require)
                                (reduce #(assoc %1 %2 %2) requires js-require)

                                :else requires)
                               ))
                           (:requires ast {})
                           (:use-macros ast))]
      (assoc ast :requires requires))
    ast))


