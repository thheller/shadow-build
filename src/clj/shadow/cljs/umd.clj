(ns shadow.cljs.umd
  (:require [shadow.cljs.build :as cljs]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [cljs.compiler :as comp]
            [shadow.cljs.node :as node])
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

(defn- infer-public-dir [{:keys [umd-options] :as state}]
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
       [logger (format "Flushing UMD file: %s" output-file)]

       (let [{:keys [output prepend append source-map-name name js-name] :as mod} (first modules)]
         (let [out (str prepend unoptimizable output append)
               out (umd-wrap out)]
           (spit output-file out)
           ))))
   state))

;; I hope no one ever sees this ...
(defn- provide->js-obj [^String provide]
  (let [parts (str/split provide #"\.")]
    (loop [path nil
           parts parts
           result []]
      (let [part (first parts)]
        (cond
          (nil? part)
          result

          (= "goog" part)
          (recur part (rest parts) result)

          :else
          (let [next-path (if path (str path "." part) part)
                token (str next-path " = goog.getObjectByName('" next-path "');")
                token (if path
                        token
                        (str "var " token))]
            (recur
              next-path
              (rest parts)
              (conj result token)))))
      )))

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
             (map comp/munge)
             (map str)
             (sort))

        goog-provides
        (->> provides
             (map #(str "goog.provide('" % "');"))
             (str/join "\n"))

        local-names
        (->> provides
             (mapcat provide->js-obj)
             (distinct)
             (str/join "\n"))]

    (str/join "\n"
      [goog-provides
       local-names
       ;; noop all other provides
       "goog.provide = function() {};\n"
       ])))


(defn flush-unoptimized-module
  [{:keys [umd-exports build-modules unoptimizable node-global-prefix] :as state}]
  (when-not (seq build-modules)
    (throw (ex-info "flush before compile?" {})))

  (cljs/with-logged-time
    [(:logger state) "Flushing sources"]
    (cljs/flush-sources-by-name state (mapcat :sources build-modules)))

  (cljs/with-logged-time
    [(:logger state) "Flushing unoptimized UMD module"]

    ;; flush fake modules
    (let [mod (first build-modules)
          {:keys [default js-name prepend prepend-js append-js sources web-worker]} mod]

      (let [target (StringWriter.)
            append-to-target
            (fn [^String text]
              (when (seq text)
                (.write target text)))]

        (append-to-target prepend)
        (append-to-target prepend-js)
        (append-to-target unoptimizable)
        ;; make sure goog is the global.goog so "var goog" isn't something else
        (when (not= node-global-prefix "global")
          (append-to-target (str "\n" node-global-prefix " = {};"))
          ;; properties that closure accesses via goog.global
          ;; apparently this is not allowed, causes "illegal invocation" when
          ;; calling global.THING.setTimeout instead of global.setTimeout?
          #_(doseq [prop ["setTimeout" "clearTimeout" "setInterval" "clearInterval" "console"]]
              (append-to-target (str "\n" node-global-prefix "." prop "=global." prop ";"))
              )

          ;; just create delegate functions
          (append-to-target (str "\n" node-global-prefix ".setTimeout = function(cb, ms) { global.setTimeout(cb, ms); }"))
          (append-to-target (str "\n" node-global-prefix ".setInterval = function(cb, ms) { global.setInteval(cb, ms); }"))
          (append-to-target (str "\n" node-global-prefix ".clearTimeout = function(id) { global.clearTimeout(id); }"))
          (append-to-target (str "\n" node-global-prefix ".clearInterval = function(id) { global.clearInterval(id); }"))
          )
        (append-to-target (str "\nvar goog = " node-global-prefix ".goog = {};"))
        (append-to-target (str "\nvar SHADOW_MODULES = " node-global-prefix ".SHADOW_MODULES = {};"))
        (append-to-target (str "\nvar CLOSURE_IMPORT_SCRIPT = " node-global-prefix ".CLOSURE_IMPORT_SCRIPT = function(src, opt_sourceText) { console.log(\"BROKEN IMPORT\", src); };\n"))
        (append-to-target
          (node/replace-goog-global
            (node/closure-defines-and-base state)
            node-global-prefix))

        ;; FIXME: this only really needs to var the top level (eg. cljs, not cljs.core)
        (let [node-compat (generate-node-compat-namespaces state mod)]
          (append-to-target node-compat))

        ;; FIXME: source-map! (same index stuff from flush-unoptimized-compact)
        ;; need to check if node supports those

        (doseq [src-name sources
                :let [{:keys [output name js-name] :as rc} (get-in state [:sources src-name])]]
          (append-to-target (str "// SOURCE=" name "\n"))
          (append-to-target (str "goog.dependencies_.written[" (pr-str js-name) "] = true;\n"))
          (append-to-target (str (str/trim output) "\n")))

        (append-to-target (str "\n\nSHADOW_MODULES[" (-> mod :name str pr-str) "] = true;\n"))
        (append-to-target append-js)

        (append-to-target "\nmodule.exports = shadow_umd_helper.get_exports();")

        (let [output-file (umd-output-file state)]
          (spit output-file (str target))
          ))))

  ;; return unmodified state
  state)


(defn create-module
  ([state exports]
   (create-module state exports {}))
  ([state exports opts]
   {:pre [(map? exports)
          (seq exports)
          (map? opts)]}
   (let [entries (->> exports
                      (vals)
                      (map namespace)
                      (map symbol)
                      (into #{}))

         requires (set/union #{'cljs.core} entries)

         umd-helper
         {:name "shadow_umd_helper.cljs"
          :js-name "shadow_umd_helper.js"
          :type :cljs
          :provides #{'shadow-umd-helper}
          :requires requires
          :require-order (into [] requires)
          :ns 'shadow-umd-helper
          :input (atom [`(~'ns ~'shadow-umd-helper
                           (:require ~@(mapv vector entries)))
                        `(defn ~(with-meta 'get-exports {:export true}) []
                           (cljs.core/js-obj ~@(->> exports (mapcat (fn [[k v]] [(name k) v])))))])
          :last-modified (System/currentTimeMillis)}]

     (-> state
         (assoc :umd-options opts)
         (assoc :umd-exports exports)
         (cljs/merge-resource umd-helper)
         (infer-public-dir)
         (cljs/configure-module :umd '[shadow-umd-helper] #{})))))
