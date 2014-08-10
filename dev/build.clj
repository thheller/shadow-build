(ns build
  (:require [shadow.cljs.build :as cljs]
            [clojure.java.io :as io]))

(defn define-modules [state]
  (-> state
      (cljs/step-configure-module
       :cljs ;; module name
       ['cljs.core] ;; module mains, a main usually contains exported functions or code that just runs
       #{}) ;; module dependencies
      (cljs/step-configure-module :basic ['basic] #{:cljs})
      (cljs/step-configure-module :other ['other] #{:cljs})
      ))

(defn dev
  "build the project, wait for file changes, repeat"
  [& args]
  (let [state (-> (cljs/init-state)
                  ;; you can use a custom logger (log4j, logback) the default just println
                  #_ (assoc :logger (reify cljs/BuildLog
                                      (log-warning [_ log-string])
                                      (log-progress [_ log-string])
                                      (log-time-start [_ log-string])
                                      (log-time-end [_ log-string time-in-ms])))
                  (cljs/enable-source-maps)
                  (assoc :optimizations :none
                         :pretty-print true
                         :work-dir (io/file "target/cljs-work") ;; temporary output path, not really needed
                         :public-dir (io/file "target/cljs") ;; where should the output go
                         :public-path "target/cljs") ;; whats the path the html has to use to get the js?
                  (cljs/step-find-resources-in-jars) ;; finds cljs,js in jars from the classpath
                  (cljs/step-find-resources "lib/js-closure" {:reloadable false})
                  (cljs/step-find-resources "test-data") ;; find cljs in this path
                  (cljs/step-finalize-config) ;; shouldn't be needed but is at the moment
                  (cljs/step-compile-core)    ;; compile cljs.core
                  (define-modules)
                  )]
    
    ;; compile, flush, reload, repeat
    (loop [state state]
      (recur (-> state
                 (cljs/step-compile-modules)
                 (cljs/flush-to-disk)
                 (cljs/flush-unoptimized)
                 (cljs/wait-and-reload!)
                 ))))
  ;; never really gets here, ctrl+c to stop
  :done)

(defn production
  "production level build, no source maps, advanced opt"
  [& args]
  (-> (cljs/init-state)
      (cljs/enable-source-maps)
      (assoc :optimizations :advanced
             :pretty-print false
             :work-dir (io/file "target/cljs-work")
             :public-dir (io/file "target/cljs")
             :public-path "target/cljs")
      (cljs/step-find-resources-in-jars)
      (cljs/step-find-resources "lib/js-closure" {:reloadable false})
      (cljs/step-find-resources "test-data")
      (cljs/step-finalize-config)
      (cljs/step-compile-core)
      (define-modules)
      (cljs/step-compile-modules)
      (cljs/closure-optimize)
      (cljs/flush-to-disk) ;; flush all generated sources to disk (although not used)
      (cljs/flush-modules-to-disk))

  ;; closure compiler thread keeps the jvm alive but all work is done, dunno why this is
  (shutdown-agents)
  (System/exit 0))

(defn progress [state message]
  (println message)
  state)

(defn separate
  "not terribly useful, but shows how 3 separate modules can be built in one go"
  [& args]
  (-> (cljs/init-state)
      (cljs/enable-source-maps)
      (assoc :optimizations :advanced
             :pretty-print false
             :work-dir (io/file "target/cljs-work")
             :public-dir (io/file "target/cljs")
             :public-path "target/cljs")
      (cljs/step-find-resources-in-jars)
      (cljs/step-find-resources "lib/js-closure" {:reloadable false})
      (cljs/step-find-resources "test-data")
      (cljs/step-finalize-config)
      (cljs/step-compile-core)

      (progress "Compiling cljs ...")
      (cljs/step-configure-module :cljs ['cljs.core] #{})
      (cljs/step-compile-modules)
      (cljs/closure-optimize)
      (cljs/flush-modules-to-disk)

      (progress "Compiling other ...")
      (cljs/reset-modules)
      (cljs/step-configure-module :other ['other] #{})
      (cljs/step-compile-modules)
      (cljs/closure-optimize)
      (cljs/flush-modules-to-disk)

      (progress "Compiling basic ...")
      (cljs/reset-modules)
      (cljs/step-configure-module :basic ['basic] #{})
      (cljs/step-compile-modules)
      (cljs/closure-optimize)
      (cljs/flush-modules-to-disk)) 

  ;; closure compiler thread keeps the jvm alive but all work is done, dunno why this is
  (shutdown-agents)
  (System/exit 0)
  :done)

(defn workers
  [& args]
  (-> (cljs/init-state)
      (cljs/enable-source-maps)
      (assoc :optimizations :advanced
             :pretty-print false
             :work-dir (io/file "target/cljs-work")
             :public-dir (io/file "target/cljs")
             :public-path "target/cljs")
      (cljs/step-find-resources-in-jars)
      (cljs/step-find-resources "lib/js-closure" {:reloadable false})
      (cljs/step-find-resources "test-workers")
      (cljs/step-finalize-config)
      (cljs/step-compile-core)

      (cljs/step-configure-module :cljs ['cljs.core] #{})
      (cljs/step-configure-module :page ['page] #{:cljs})
      (cljs/step-configure-module :worker1 ['worker1] #{:cljs} {:web-worker true})
      (cljs/step-configure-module :worker2 ['worker2] #{:cljs} {:web-worker true})

      (cljs/step-compile-modules)
      (cljs/closure-optimize)
      (cljs/flush-modules-to-disk)))
