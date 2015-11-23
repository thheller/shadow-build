(ns shadow.cljs.umd
  (:require [shadow.cljs.build :as cljs]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))


(defn flush-module
  [{modules :optimized :keys [logger unoptimizable] :as state} output-to]
  (when-not (seq modules)
    (throw (ex-info "flush before optimize?" {})))

  (when-not (= 1 (count modules))
    (throw (ex-info "can only work with one module for now" {})))

  (cljs/with-logged-time
    [logger (format "Flushing UMD file: %s" output-to)]

    (let [output-file (io/file output-to)]
      (io/make-parents output-file)

      (let [{:keys [output prepend append source-map-name name js-name] :as mod} (first modules)]
        ;; https://github.com/umdjs/umd/blob/master/templates/returnExports.js
        (let [wrapper (slurp (io/resource "shadow/cljs/umd_exports.txt"))
              out (str unoptimizable prepend output append)
              out (str/replace wrapper "//CLJS-HERE" out)]
          (spit output-file out)
          ))))
  state)

(defn create-module [state exports]
  (let [entries (->> exports
                     (vals)
                     (map namespace)
                     (map symbol)
                     (into #{}))

        umd-helper
        {:name "shadow_umd_helper.cljs"
         :js-name "shadow_umd_helper.js"
         :type :cljs
         :provides #{'shadow-umd-helper}
         :requires (set/union #{'cljs.core} entries)
         :ns 'shadow-umd-helper
         :input (atom [`(~'ns ~'shadow-umd-helper
                          (:require ~@(mapv vector entries)))
                       `(defn ~(with-meta 'get-exports {:export true}) []
                          (cljs.core/js-obj ~@(->> exports (mapcat (fn [[k v]] [(name k) v])))))])
         :last-modified (System/currentTimeMillis)}]

    (-> state
        (cljs/merge-resource umd-helper)
        (cljs/configure-module :umd '[shadow-umd-helper] #{}))))
