(ns repl
  (:require [shadow.cljs.build :as cljs]
            [clojure.java.io :as io]
            [shadow.sass :as sass]
            [shadow.cljs.live-reload :as live-reload]))



(defn simple
  [& args]
  (-> (cljs/init-state)
      (cljs/enable-source-maps)
      (assoc :optimizations :none
             :pretty-print true
             :work-dir (io/file "target/cljs-work")
             :cache-dir (io/file "target/cljs-cache")
             :cache-level :jars
             :public-dir (io/file "cljs-data/dummy/out")
             :public-path "out")
      (cljs/step-find-resources-in-jars)
      (cljs/step-find-resources "src/cljs")
      (cljs/step-find-resources "cljs-data/dummy/src")
      (cljs/step-find-resources "cljs-data/dummy/test")

      (cljs/step-finalize-config)

      (cljs/step-configure-module :cljs ['cljs.core] #{})
      (cljs/step-configure-module :basic ['basic] #{:cljs})
      (cljs/step-configure-module :other ['other] #{:cljs})

      (live-reload/start-repl
        {;; :before-load 'demo.app/stop
         ;; :after-load 'demo.app/start
         ;; :host "localhost" ;; optional, defaults to localhost
         ;; :port 8889 ;; optional, defaults to random open port
         }
        (fn [state modified]
          (-> state
              (cljs/step-compile-modules)
              (cljs/flush-unoptimized)
              )))))