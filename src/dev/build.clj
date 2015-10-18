(ns build
  (:require [shadow.cljs.build :as cljs]
            [shadow.cljs.node :as node]
            [clojure.java.io :as io]
            ))

(defn workers
  [& args]
  (-> (cljs/init-state)
      (cljs/enable-source-maps)
      (assoc :optimizations :advanced
             :pretty-print false
             :work-dir (io/file "target/cljs-work")
             :public-dir (io/file "cljs-data/workers/out")
             :public-path "/out")
      (cljs/find-resources-in-classpath)
      (cljs/find-resources "cljs-data/workers/src")
      (cljs/finalize-config)
      (cljs/configure-module :cljs ['cljs.core] #{})
      (cljs/configure-module :page ['page] #{:cljs})
      (cljs/configure-module :worker1 ['worker1] #{:cljs} {:web-worker true})
      (cljs/configure-module :worker2 ['worker2] #{:cljs} {:web-worker true})

      (cljs/compile-modules)
      (cljs/flush-unoptimized)
      
      ;; (cljs/closure-optimize)
      ;; (cljs/flush-modules-to-disk)
      ))

(defn dev
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
      (cljs/find-resources-in-classpath)
      (cljs/find-resources "cljs-data/dummy/src")
      (cljs/find-resources "cljs-data/dummy/test")

      (cljs/finalize-config)

      (cljs/configure-module :cljs ['cljs.core] #{})
      (cljs/configure-module :basic ['basic] #{:cljs})
      (cljs/configure-module :other ['other] #{:cljs})

      (cljs/watch-and-repeat!
        (fn [state modified]
          (-> state
              (cljs/compile-modules)
              (cljs/flush-unoptimized)
              (cond->
                ;; first pass, run all tests
                (empty? modified)
                (node/execute-all-tests!)
                ;; only execute tests that might have been affected by the modified files
                (not (empty? modified))
                (node/execute-affected-tests! modified))
              ))) ))

