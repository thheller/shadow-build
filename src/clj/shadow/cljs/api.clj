(ns shadow.cljs.api
  (:require [shadow.cljs.build :as cljs]
            [clojure.java.io :as io]
            [clojure.pprint :refer (pprint)]))


(defn add-source-paths [state source-paths]
  (reduce cljs/step-find-resources state source-paths))

(defn define-modules [state modules]
  (reduce (fn [state {:keys [main name depends-on] :as args}]
            (cljs/step-configure-module
             state
             name
             (cond
              (symbol? main)
              [main]

              :default 
              main)
             (or depends-on #{})
             args))
          state
          modules))

(defn build-dev
  "build the project, wait for file changes, repeat"
  [{:keys [source-paths public-dir public-path modules] :as config} & args]
  (when (nil? config)
    (throw (ex-info "no cljs config, wrong key?")))
  ;; FIXME: validate config!

  (loop [state (-> (cljs/init-state)
                   (merge (dissoc config :modules :source-paths :public-dir :public-path))
                   (cljs/enable-source-maps)
                   (assoc :optimizations :none
                          :pretty-print true
                          :work-dir (io/file "target/cljs-work")
                          :public-dir (io/file public-dir)
                          :public-path public-path)
                   (cljs/step-find-resources-in-jars)
                   (add-source-paths source-paths)
                   (cljs/step-finalize-config)
                   (cljs/step-compile-core)
                   (define-modules modules))]

    (recur (try
             (-> state
                 (cljs/step-compile-modules)
                 (cljs/flush-unoptimized)
                 (cljs/wait-and-reload!))
             (catch Throwable t
               (println (str "COMPILATION FAILED: " t))
               (.printStackTrace t)
               (cljs/wait-and-reload! state)))))

  ;; never really gets here, ctrl+c to stop
  :done)

(defn build-prod
  "build the project with advanced optimizations"
  [{:keys [source-paths public-dir public-path modules] :as config} & args]
  (when (nil? config)
    (throw (ex-info "no cljs config, wrong key?")))
  ;; FIXME: validate config!

  (-> (cljs/init-state)
      (merge (dissoc config :modules :source-paths :public-dir :public-path))
      (assoc :optimizations :advanced
             :pretty-print false
             :work-dir (io/file "target/cljs-work-prod")
             :public-dir (io/file public-dir)
             :public-path public-path)
      (cljs/step-find-resources-in-jars)
      (add-source-paths source-paths)
      (cljs/step-finalize-config)
      (cljs/step-compile-core)
      (define-modules modules)
      (cljs/step-compile-modules)
      (cljs/closure-optimize)
      (cljs/flush-modules-to-disk))

  ;; closure compiler thread keeps the jvm alive but all work is done, dunno why this is
  (shutdown-agents)
  (System/exit 0))


