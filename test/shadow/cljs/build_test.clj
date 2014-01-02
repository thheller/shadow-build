(ns shadow.cljs.build-test
  (:use clojure.test
        shadow.fix-test)
  (:import [java.io File StringWriter]
           [com.google.javascript.jscomp JSModule SourceFile])
  (:require [shadow.cljs.build :as cljs]
            [shadow.cljs.passes :as p]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.env :as env]
            [cljs.source-map :as sm]
            [cljs.closure :as closure]
            [clojure.data.json :as json]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [loom.graph :as lg]
            [loom.alg :as la]
            ))

;; from cljs.analyzer.utils which is only in head yet
(defn simplify-env [_ {:keys [op] :as ast}]
  (let [env (:env ast)
        ast (if (= op :fn)
              (assoc ast :methods
                (map #(simplify-env nil %) (:methods ast)))
              ast)]
    (assoc (dissoc ast :env)
      :env {:context (:context env)})))

(defn elide-children [_ ast]
  (dissoc ast :children))

(defn to-ast
  ([form] (to-ast 'cljs.user form))
  ([ns form]
     (let [env (assoc-in (ana/empty-env) [:ns :name] ns)]
       (binding [ana/*passes* [elide-children
                               simplify-env
                               p/macro-js-requires
                               ana/infer-type]
                 ana/*cljs-ns* 'cljs.user]
         (ana/analyze env form)))))

;; 

(defmacro ^{:js-require 'goog.math} a-macro [] :yo)

(deftest test-macro-js-require
  (let [ast (to-ast '(ns some.where
                       (:require-macros [shadow.cljs.build-test :refer (a-macro)])))]
    (is (get-in ast [:requires 'goog.math]))))


(deftest test-base-js-foobar ;; base.js has a goog.provide in a comment
  (let [state (-> (cljs/init-state)
                  (cljs/step-find-resources-in-jars)
                  (cljs/step-find-resources "lib/js-closure"))]

    (-> state
        (get-in [:sources "goog/base.js"])
        (dissoc :js-source :source)
        (pprint)
        )))

;; this needs real testing

(deftest ^:wip test-js-env
  (binding [*err* *out*]
    (let [state (-> (user/resume-from
                     (-> (cljs/init-state)
                         (cljs/enable-source-maps)
                         (cljs/step-find-resources-in-jars)
                         (cljs/step-find-resources "lib/js-closure" {:reloadable false})
                         (cljs/step-find-resources "test-data")
                         (cljs/step-find-resources "test-workers")
                         (assoc :optimizations :whitespace
                                :pretty-print true
                                :work-dir (io/file "target/cljs-work")
                                :public-dir (io/file "target/cljs")
                                :public-path "target/cljs")
                         (cljs/step-finalize-config)
                         (cljs/step-compile-core)))

                    (cljs/step-reload-modified)
                    ;; (cljs/step-configure-module :cljs ['cljs.core] #{})
                    ;;(cljs/step-configure-module :basic ['basic] #{:cljs})
                    ;;(cljs/step-configure-module :other ['other] #{:cljs})
                    ;;(step-flush-to-disk)
                    (cljs/step-configure-module :cljs ['cljs.core] #{})
                    (cljs/step-configure-module :page ['page] #{:cljs})
                    (cljs/step-configure-module :worker1 ['worker1] #{:cljs} {:web-worker true})
                    (cljs/step-configure-module :worker2 ['worker2] #{:cljs} {:web-worker true})


                    )]
      
      (prn [:count-sources (count (:sources state))])

      (let [state (-> state
                      (cljs/step-compile-modules)
                      ;; (cljs/flush-unoptimized)
                      (cljs/closure-optimize)
                      (cljs/flush-modules-to-disk)
                      )]

        (pprint (->> state
                     :optimized
                     (map #(dissoc % :prepend-js :js-source :source-map :source-map-json))))
        ))))












