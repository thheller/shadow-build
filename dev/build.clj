(ns build
  (:require [shadow.cljs.build :as cljs]
            [clojure.java.io :as io]))

;; hmm dogfood

(defn dev
  "build the project, wait for file changes, repeat"
  [& args]
  (let [state (-> (cljs/init-state)
                  (cljs/enable-source-maps)
                  (assoc :optimizations :none
                         :pretty-print true
                         :work-dir (io/file "target/cljs-work")
                         :public-dir (io/file "target/cljs")
                         :public-path "target/cljs"
                         :fingerprint-modules true)
                  (cljs/step-find-resources-in-jars)
                  (cljs/step-find-resources "lib/js-closure" {:reloadable false})
                  (cljs/step-find-resources "test-data")
                  (cljs/step-finalize-config)
                  (cljs/step-compile-core)
                  (cljs/step-configure-module :cljs ['cljs.core] #{})
                  (cljs/step-configure-module :basic ['basic] #{:cljs})
                  (cljs/step-configure-module :other ['other] #{:cljs})
                  )]
    
    (loop [state state]
      (recur (-> state
                 (cljs/step-compile-modules)
                 (cljs/flush-unoptimized)
                 (cljs/wait-and-reload!)
                 ))))
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
      (cljs/step-configure-module :cljs ['cljs.core] #{})
      (cljs/step-configure-module :basic ['basic] #{:cljs})
      (cljs/step-configure-module :other ['other] #{:cljs})
      (cljs/step-compile-modules)
      (cljs/closure-optimize)
      (cljs/flush-to-disk) ;; flush all generated sources to disk (although not used)
      (cljs/flush-modules-to-disk))

  ;; closure compiler thread keeps the jvm alive but all work is done, dunno why this is
  (shutdown-agents)
  (System/exit 0)
  :done)

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
