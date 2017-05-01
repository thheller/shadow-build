(ns shadow.cljs.umd
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [shadow.cljs.build :as cljs]
            [shadow.cljs.output :as output]
            [shadow.cljs.node :as node]
            [shadow.cljs.util :as util]))

(defn flush-module [state]
  (node/flush-optimized state))

(defn flush-unoptimized-module
  [state]
  (node/flush-unoptimized state))

(defn create-module
  ([state exports]
   (create-module state exports {}))
  ([state exports {:keys [output-to public-dir] :as opts}]
   {:pre [(util/compiler-state? state)
          (map? exports)
          (seq exports)
          (map? opts)]}
   (let [entries
         (->> exports
              (vals)
              (map namespace)
              (map symbol)
              (into #{}))

         requires
         (set/union #{'cljs.core} entries)

         umd-helper
         {:name "shadow/umd_helper.cljs"
          :js-name "shadow/umd_helper.js"
          :type :cljs
          :provides #{'shadow.umd-helper}
          :requires requires
          :require-order (into [] requires)
          :ns 'shadow.umd-helper
          :input (atom [`(~'ns ~'shadow.umd-helper
                           (:require ~@(mapv vector entries)))
                        `(defn ~(with-meta 'get-exports {:export true}) []
                           (cljs.core/js-obj ~@(->> exports (mapcat (fn [[k v]] [(name k) v])))))])
          :last-modified (System/currentTimeMillis)}

         output-to
         (io/file output-to)

         output-name
         (.getName output-to)

         module-name
         (-> output-name (str/replace #".js$" "") (keyword))

         public-dir
         (if (seq public-dir)
           (io/file public-dir)
           (io/file "target" "shadow-umd"))

         node-config
         {:output-to output-to
          :public-dir public-dir}

         ;; based on https://github.com/umdjs/umd/blob/master/templates/returnExports.js
         [prepend append]
         (-> (slurp (io/resource "shadow/cljs/umd_exports.txt"))
             (str/split #"//CLJS-HERE"))]

     (-> state
         (assoc ::options opts)
         (assoc ::exports exports)
         (assoc :node-config node-config)
         (assoc :public-dir public-dir)
         (cljs/merge-resource umd-helper)
         (cljs/configure-module :umd '[shadow.umd-helper] #{}
           {:prepend prepend
            :append append})))))
