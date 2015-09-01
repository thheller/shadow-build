(ns shadow.cljs.api
  (:import (java.util UUID))
  (:require [shadow.cljs.build :as cljs]
            [clojure.java.io :as io]
            [clojure.pprint :refer (pprint)]
            [cljs.compiler :as comp]
            [shadow.cljs.live-reload :as lr]))

(defn- add-source-paths [state source-paths]
  (reduce cljs/step-find-resources state source-paths))

(defn- define-modules [state modules]
  (reduce (fn [state {:keys [main name depends-on prepend] :as args}]
            (cljs/step-configure-module
              state
              name
              (cond
                (symbol? main)
                [main]

                :default
                main)
              (or depends-on #{})
              (-> args
                  (cond->
                    prepend
                    (assoc :prepend (slurp (io/resource prepend)))))))
          state
          modules))

(defn build-dev
  {:doc "build the project, wait for file changes, repeat"
   :deprecated true}
  [{:keys [live-reload source-paths public-dir public-path modules test] :as config} & args]
  (when (nil? config)
    (throw (ex-info "no cljs config, wrong key?" {})))
  ;; FIXME: validate config!
  
  (-> (cljs/init-state)
      (merge (dissoc config :modules :source-paths :public-dir :public-path))
      (cljs/enable-source-maps)
      (assoc :optimizations :none
             :use-file-min false
             :pretty-print true
             :work-dir (io/file "target/cljs-work")
             :cache-dir (io/file "target/cljs-cache")
             :cache-level :jars
             :public-dir (io/file public-dir)
             :public-path public-path)
      (cljs/step-find-resources-in-jars)
      (add-source-paths source-paths)
      (cljs/step-finalize-config)
      (cljs/step-compile-core)
      (define-modules modules)
      (lr/setup live-reload)
      (cljs/watch-and-repeat!
       (lr/wrap
        (fn [state modified]
          (-> state
              (cljs/step-compile-modules)
              (cljs/flush-unoptimized)
              )))))
  ;; never really gets here, ctrl+c to stop
  :done)

(defn build-prod
  {:doc "build the project with advanced optimizations"
   :deprecated true}
  [{:keys [source-paths public-dir public-path modules] :as config} & args]
  (when (nil? config)
    (throw (ex-info "no cljs config, wrong key?" {})))
  ;; FIXME: validate config!

  (-> (cljs/init-state)
      (merge (dissoc config :modules :source-paths :public-dir :public-path))
      (assoc :optimizations :advanced
             :pretty-print false
             :work-dir (io/file "target/cljs-work-prod")
             :public-dir (io/file public-dir)
             :public-path public-path)
      (cljs/enable-emit-constants)
      (cljs/step-find-resources-in-jars)
      (add-source-paths source-paths)
      (cljs/step-finalize-config)
      (cljs/step-compile-core)
      (define-modules modules)
      (cljs/step-compile-modules)
      (cljs/closure-optimize)
      (cljs/flush-modules-to-disk)))


