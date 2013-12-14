(ns shadow.cljs.passes)

;; from clojure jvm/clojure/lang/Compiler.java
;; some of those shouldnt appear ever, better safe than sorry tho
(def fn-name-replace-chars
  {\- "_",
   \. "_DOT_",
   \: "_COLON_",
   \+ "_PLUS_",
   \> "_GT_",
   \< "_LT_",
   \= "_EQ_",
   \~ "_TILDE_",
   \! "_BANG_",
   \@ "_CIRCA_",
   \# "_SHARP_",
   \', "_SINGLEQUOTE_",
   \" "_DOUBLEQUOTE_",
   \% "_PERCENT_",
   \^ "_CARET_",
   \& "_AMPERSAND_",
   \* "_STAR_",
   \| "_BAR_",
   \{ "_LBRACE_",
   \} "_RBRACE_",
   \[ "_LBRACK_",
   \] "_RBRACK_",
   \/ "_SLASH_",
   \\ "_BSLASH_",
   \? "_QMARK_"})

(defn make-fn-name [{:keys [name] :as ast}]
  (let [sb (StringBuilder.)
        name (cond
              (nil? name) 'anon
              (map? name) (:name name)
              (symbol? name) name
              :else (throw (ex-info "invalid name" {:name name})))
        ns (get-in ast [:env :ns :name])]
    (doseq [c (str "fn-" ns "/" name "-" (get-in ast [:env :line] 0) "-" (get-in ast [:env :column] 0))]
      (.append sb (fn-name-replace-chars c c)))
    (symbol (.toString sb))
    ))


;; this is still buggy, breaks variadics but not using :locals correctly
(defn name-every-function-pass [_ {:keys [locals name op] :as ast}]
  (if (or (not= :fn op)
          (::named ast))
    ast
    (let [fn-name (make-fn-name ast)]
      (assoc ast
        :name fn-name
        ::named true))))

;; this works quite fine
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


