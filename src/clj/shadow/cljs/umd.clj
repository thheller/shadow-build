(ns shadow.cljs.umd
  (:require [shadow.cljs.build :as cljs]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [cljs.compiler :as comp]
            [shadow.cljs.node :as node]
            [shadow.cljs.util :as util])
  (:import (java.io StringWriter File)))

(defn- umd-wrap [text]
  ;; https://github.com/umdjs/umd/blob/master/templates/returnExports.js
  (let [wrapper (slurp (io/resource "shadow/cljs/umd_exports.txt"))]
    (str/replace wrapper "//CLJS-HERE" text)
    ))

(defn- umd-output-file ^File [{:keys [umd-options] :as state}]
  (let [output-to (:output-to umd-options)
        _ (when-not (and (string? output-to) (seq output-to))
            (throw (ex-info "no umd :output-to option set" umd-options)))
        output-file (io/file output-to)]

    (io/make-parents output-file)
    output-file))

(defn infer-public-dir [{:keys [umd-options] :as state}]
  (assoc state :public-dir (.getParentFile (umd-output-file state))))

(defn flush-module
  ([state output-to]
   (-> state
       (assoc-in [:umd-options :output-to] output-to)
       (infer-public-dir)
       (flush-module)))
  ([{modules :optimized :keys [logger unoptimizable] :as state}]
   (when-not (seq modules)
     (throw (ex-info "flush before optimize?" {})))

   (when-not (= 1 (count modules))
     (throw (ex-info "can only work with one module for now" {})))

   (let [output-file (umd-output-file state)]
     (cljs/with-logged-time
       [state {:type ::flush
               :optimized true
               :output-file output-file}]

       (let [{:keys [output prepend append source-map-name name js-name] :as mod} (first modules)]
         (let [out (str prepend unoptimizable output append)
               out (umd-wrap out)]
           (spit output-file out)
           ))))
   state))

(defn- generate-node-compat-namespaces [state mod]
  ;; eagerly emits all goog.provide statements
  ;; and then pulls all names into the local scope so node can work with them

  ;; goog.provide("goog.dom.NodeType");
  ;; goog.dom.NodeType = ...
  ;; does not work in node since goog.provide will export the name into global
  ;; but we are not in a global scope so goog.dom doesn't exist when trying to
  ;; assign goog.dom.NodeType
  ;; so we need to construct all namespaces manually
  ;; could use :simple to do that for us but that is pretty slow and annoying during development
  ;; since we know all the names we can just create them
  (let [provides
        (->> (:sources mod)
             (map #(get-in state [:sources % :provides]))
             (reduce set/union)
             (sort))

        goog-provides
        (->> provides
             (map comp/munge)
             (map #(str "goog.provide('" % "');"))
             (str/join "\n"))

        local-provides
        (->> (util/js-for-local-names provides)
             (str/join "\n"))]

    (str/join "\n"
      [goog-provides
       local-provides
       ;; noop all other provides
       "goog.provide = function() {};\n"
       ])))


(defn flush-unoptimized-module
  [state]
  (node/flush-unoptimized state))

(defn create-module
  ([state exports]
   (create-module state exports {}))
  ([state exports {:keys [output-to public-dir] :as opts}]
   {:pre [(cljs/compiler-state? state)
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
           (io/file "target" "shadow.umd"))

         node-config
         {:output-to output-to
          :public-dir public-dir}]

     (-> state
         (assoc ::options opts)
         (assoc ::exports exports)
         (assoc :node-config node-config)
         (assoc :public-dir public-dir)
         (cljs/merge-resource umd-helper)
         (cljs/configure-module :umd '[shadow.umd-helper] #{}
           {:append-js "\nmodule.exports = shadow.umd_helper.get_exports();"})))))
