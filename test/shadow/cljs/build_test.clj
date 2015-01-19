(ns shadow.cljs.build-test
  (:use clojure.test
        shadow.fix-test)
  (:require [shadow.cljs.build :as cljs]
            [cljs.analyzer :as ana]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str]
            [clojure.java.io :as io]
            ))

(deftest test-initial-scan
  (.setLastModified (io/file "dev/shadow/test_macro.clj") 0)
  (let [state (-> (cljs/init-state)
                  (cljs/step-find-resources-in-jars)
                  (cljs/step-find-resources "test-data")
                  (cljs/step-finalize-config))]
    (is (empty? (cljs/scan-for-modified-files state)))
    (.setLastModified (io/file "dev/shadow/test_macro.clj") (System/currentTimeMillis))
    (let [modded (cljs/scan-for-modified-files state)
          state (cljs/reload-modified-files! state modded)]
      (is (empty? (cljs/scan-for-modified-files state)))
      )))

;; this needs real testing

(deftest test-js-env
  (let [state (-> (cljs/init-state)
                  (cljs/enable-source-maps)
                  (cljs/step-find-resources-in-jars)
                  (cljs/step-find-resources "cljs-data/dummy/src")
                  (assoc :optimizations :none
                         :pretty-print true
                         :work-dir (io/file "target/cljs-work")
                         :cache-dir (io/file "target/cljs-cache")
                         :cache-level :jars
                         :public-dir (io/file "target/cljs")
                         :public-path "target/cljs")
                  (cljs/step-finalize-config)
                  (cljs/step-configure-module :cljs ['cljs.core] #{})
                  (cljs/step-configure-module :basic ['basic] #{:cljs})
                  (cljs/step-configure-module :other ['other] #{:cljs})
                  (cljs/step-compile-modules)
                  ;;(step-flush-to-disk)
                  (cljs/flush-unoptimized)
                  ;; (cljs/closure-optimize)
                  ;; (cljs/flush-modules-to-disk)
                  ;;(cljs/step-configure-module :cljs ['cljs.core] #{})
                  ;;(cljs/step-configure-module :page ['page] #{:cljs})
                  ;;(cljs/step-configure-module :worker1 ['worker1] #{:cljs} {:web-worker true})
                  ;;(cljs/step-configure-module :worker2 ['worker2] #{:cljs} {:web-worker true})
                  )]

    ))

(deftest test-reloading
  (let [file-a (io/file "target/reload-test/test_a.cljs")
        file-b (io/file "target/reload-test/test_b.cljs")
        foo-fn "(defn ^:export foo[] :bar)"]
    (io/make-parents file-a)

    (doseq [file [file-a file-b]
            :when (.exists file)]
      (.delete file))

    (spit file-a (str/join "\n" ["(ns test-a)"
                                 foo-fn]))

    (let [state (-> (cljs/init-state)
                    (cljs/enable-source-maps)
                    (cljs/step-find-resources-in-jars)
                    (cljs/step-find-resources "target/reload-test")
                    (assoc :optimizations :whitespace
                           :pretty-print true
                           :work-dir (io/file "target/cljs-work")
                           :public-dir (io/file "target/cljs")
                           :public-path "target/cljs")
                    (cljs/step-finalize-config)
                    (cljs/step-compile-core)
                    (cljs/step-configure-module :test ['test-a] #{}))]

      (is (nil? (get-in state [:sources "test_b.cljs"])))

      (cljs/step-compile-modules state) ;; no error is good enough for now

      ;; wait for a bit
      ;; otherwise the spit may end up in the same millisec as the previous one
      ;; which wait-and-reload can't detect
      (Thread/sleep 50)

      ;; now we modify it to depend on test-b
      (spit file-a (str/join "\n" ["(ns test-a (:require [test-b]))"
                                   foo-fn]))
      (let [modified (cljs/scan-for-modified-files state)
            new (cljs/scan-for-new-files state)]
        (is (empty? new))
        (is (= 1 (count modified)))
        (is (= :modified (-> modified first :scan)))

        ;; empty file is :new but cannot be compiled, should produce warning, hard to test
        (spit file-b "")

        (let [state (cljs/reload-modified-files! state modified)
              new (cljs/scan-for-new-files state)
              modified (cljs/scan-for-modified-files state)]

          (is (empty? modified))
          (is (= 1 (count new)))

          (spit file-b (str/join "\n" ["(ns test-b)"
                                       foo-fn]))

          (let [new (cljs/scan-for-new-files state)
                state (cljs/reload-modified-files! state new)]
            (is (= 1 (count new)))

            ;; FIXME: test if everything is ok, no exception is good enough for now
            (cljs/step-compile-modules state)
            ))))))



(deftest test-caching
  (let [do-build (fn []
                   (-> (cljs/init-state)
                       (cljs/enable-source-maps)
                       (assoc :optimizations :none
                              :pretty-print false
                              :work-dir (io/file "target/cljs-work")
                              :cache-dir (io/file "target/cljs-cache")
                              :public-dir (io/file "target/cljs")
                              :public-path "target/cljs")
                       (cljs/step-find-resources-in-jars)
                       (cljs/step-find-resources "test-data")
                       (cljs/step-finalize-config)
                       (cljs/step-compile-core)
                       (cljs/step-configure-module :basic ['basic] #{})
                       (cljs/step-compile-modules)
                       (cljs/flush-unoptimized)))]
    (println "--- ROUND 1")
    (.setLastModified (io/file "test-data/common.cljs") 1)
    (do-build)
    ;; all files should be cached now

    (println "--- ROUND 2")
    ;; should load only cached
    (do-build)

    (println "--- ROUND 3")
    ;; touch one file which should cause a recompile of all dependents too
    (.setLastModified (io/file "test-data/common.cljs") (System/currentTimeMillis))
    (do-build)

    ;; FIXME: checkout output that basic and common were recompiled, not just common
    ))

(deftest test-optimized-build
  (-> (cljs/init-state)
      (cljs/enable-source-maps)
      (assoc :optimizations :advanced
             :pretty-print false
             :work-dir (io/file "target/cljs-work")
             :cache-dir (io/file "target/cljs-cache")
             :public-dir (io/file "target/cljs")
             :public-path "target/cljs")
      (cljs/step-find-resources-in-jars)
      (cljs/step-find-resources "cljs-data/dummy/src")
      (cljs/step-finalize-config)
      (cljs/step-compile-core)
      (cljs/step-configure-module :basic ['basic] #{})
      (cljs/step-compile-modules)
      (cljs/closure-optimize)
      (cljs/flush-modules-to-disk)))

(deftest test-dummy
  (let [s (-> (cljs/init-state)
              (assoc :optimizations :none
                     :pretty-print true
                     :work-dir (io/file "target/test-cljs-work")
                     :cache-dir (io/file "target/test-cljs-cache")
                     :cache-level :jars
                     :public-dir (io/file "target/test-cljs")
                     :public-path "target/test-cljs")
              (cljs/step-find-resources-in-jars)
              (cljs/step-find-resources "cljs-data/dummy/src")
              (cljs/step-finalize-config)
              (cljs/step-configure-module :test ['shadow.dummy] #{})
              (cljs/step-compile-modules)
              (cljs/flush-unoptimized))]
    (println (get-in s [:sources "shadow/dummy.cljs" :js-source]))))

(deftest test-dev-api
  (-> (cljs/init-state)
      (assoc :optimizations :none
             :pretty-print true
             :work-dir (io/file "target/test-cljs-work")
             :cache-dir (io/file "target/test-cljs-cache")
             :cache-level :jars
             :public-dir (io/file "target/test-cljs")
             :public-path "target/test-cljs")
      (cljs/step-find-resources-in-jars)
      (cljs/step-find-resources "cljs-data/dummy/src")
      (cljs/step-find-resources "cljs-data/dummy/test")
      (cljs/execute-affected-tests! ["basic.cljs"])))

