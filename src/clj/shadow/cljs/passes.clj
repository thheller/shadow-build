(ns shadow.cljs.passes
  (:require [clojure.pprint :refer (pprint)]
            [cljs.util :as util]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [cljs.analyzer :as ana]
            [cljs.env :as env]))

(defn load-macros
  "load all macros specified in (:require-macros [...])
   if a macro namespace of the same name as the current ns is found,
   we extract all the names of all macros so later passes have an easier time
   figuring out what might be a macro"
  [_ {:keys [op name require-macros] :as ast}]
  (let [require-macros (vals require-macros)]
    ;; FIXME: cljs.analyzer parse 'ns does this set! why? - thomas
    (set! ana/*cljs-ns* name)
    (doseq [macro-ns require-macros]
      (require macro-ns))

    (if (or (not= :ns op)
            (not (contains? (set require-macros) name)))
      ast
      ;; if we require a macro with the same name as the ns
      ;; find all macro names so we have an easier time checking if a macro
      ;; exists without looking at clojure all the time
      (let [macros (->> (ns-publics name)
                        (reduce-kv (fn [m var-name the-var]
                                     (if (.isMacro ^clojure.lang.Var the-var)
                                       (conj m var-name)
                                       m))
                                   #{}))]

        (swap! env/*compiler* assoc-in [::ana/namespaces name :macros] macros)
        (assoc ast :macros macros)
        ))))

(defn infer-macro-require
  "infer (:require [some-ns]) that some-ns may come with macros
   must be used after load-macros pass"
  [_ {:keys [op requires name] :as ast}]
  (if (or (not= :ns op)
          (empty? requires))
    ast
    (reduce (fn [ast [used-name used-ns]]
              (let [macros (get-in @env/*compiler* [::ana/namespaces used-ns :macros])]
                (if (nil? macros)
                  ast
                  (let [update-fn (fn [current]
                                    (update-in current [:require-macros] assoc used-name used-ns))]

                    (swap! env/*compiler* update-in [::ana/namespaces name] update-fn)
                    (update-fn ast))
                  )))
            ast
            requires)))


(defn infer-macro-use
  "infer (:require [some-ns :refer (something)]) that something might be a macro
   must be used after load-macros pass"
  [_ {:keys [op uses name] :as ast}]
  (if (or (not= :ns op)
          (empty? uses))
    ast
    (reduce (fn [ast [used-name used-ns]]
              (let [macros (get-in @env/*compiler* [::ana/namespaces used-ns :macros])]
                (if (or (nil? macros)
                        (not (contains? macros used-name)))
                  ast
                  (let [update-fn (fn [current]
                                    (update-in current [:use-macros] merge {used-name used-ns}))]

                    (swap! env/*compiler* update-in [::ana/namespaces name] update-fn)
                    (update-fn ast))
                  )))
            ast
            uses)))

(defn check-uses
  "checks whether (:require [other-ns :refer (something)]) exists, throws if not"
  [env {:keys [op uses] :as ast}]
  (if (not= :ns op)
    ast
    (do (doseq [[sym lib] uses]
          (when (and (= (get-in @env/*compiler* [::ana/namespaces lib :defs sym] ::not-found) ::not-found)
                     (not (contains? (get-in @env/*compiler* [::ana/namespaces lib :macros]) sym)))
            (throw
              (ana/error env (ana/error-message :undeclared-ns-form {:type "var" :lib lib :sym sym})))))
        ast)))

