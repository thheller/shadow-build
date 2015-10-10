(ns build
  (:require [shadow.cljs.build :as cljs]
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
      (cljs/step-find-resources-in-jars)
      (cljs/step-find-resources "cljs-data/workers/src")
      (cljs/step-finalize-config)
      (cljs/step-configure-module :cljs ['cljs.core] #{})
      (cljs/step-configure-module :page ['page] #{:cljs})
      (cljs/step-configure-module :worker1 ['worker1] #{:cljs} {:web-worker true})
      (cljs/step-configure-module :worker2 ['worker2] #{:cljs} {:web-worker true})

      (cljs/step-compile-modules)
      (cljs/flush-unoptimized)
      
      ;; (cljs/closure-optimize)
      ;; (cljs/flush-modules-to-disk)
      ))

(defn cold-compile-cljs
  [& args]
  (let [state
        (-> (cljs/init-state)
            (assoc :cache-level :off)
            (cljs/step-find-resources-in-jars))]
    (dotimes [i 10]
      (cljs/compile-sources state ["cljs/core.cljs"]))))

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
      (cljs/step-find-resources-in-jars)
      (cljs/step-find-resources "cljs-data/dummy/src")
      (cljs/step-find-resources "cljs-data/dummy/test")

      (cljs/step-finalize-config)
      (cljs/step-compile-core)

      (cljs/step-configure-module :cljs ['cljs.core] #{})
      (cljs/step-configure-module :basic ['basic] #{:cljs})
      (cljs/step-configure-module :other ['other] #{:cljs})

      (cljs/watch-and-repeat!
        (fn [state modified]
          (-> state
              (cljs/step-compile-modules)
              (cljs/flush-unoptimized)
              (cond->
                ;; first pass, run all tests
                (empty? modified)
                (cljs/execute-all-tests!)
                ;; only execute tests that might have been affected by the modified files
                (not (empty? modified))
                (cljs/execute-affected-tests! modified))
              ))) ))

