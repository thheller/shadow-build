(ns shadow.cljs.build-test
  (:use clojure.test
        shadow.fix-test)
  (:require [shadow.cljs.build :as cljs]
            [shadow.cljs.util :as util]
            [cljs.analyzer :as ana]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [cljs.analyzer :as a]
            [clojure.set :as set])
  (:import (java.util.regex Pattern)
           (java.io File)
           (java.net URL)))

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
                  ;; (cljs/step-find-resources "/Users/zilence/code/oss/closure-library/closure")
                  ;; (cljs/step-find-resources "/Users/zilence/code/oss/closure-library/third_party")
                  (assoc :optimizations :advanced
                         :pretty-print false
                         :work-dir (io/file "target/cljs-work")
                         :cache-dir (io/file "target/cljs-cache")
                         :cache-level :jars
                         :public-dir (io/file "target/cljs")
                         :pseudo-names true
                         :pretty-print true
                         :public-path "target/cljs")
                  (cljs/step-finalize-config)
                  (cljs/step-configure-module :loader ['goog.module.ModuleManager] #{})
                  ;; (cljs/step-configure-module :cljs ['cljs.core] #{:loader})
                  ;; (cljs/step-configure-module :basic ['basic] #{:cljs})
                  ;; (cljs/step-configure-module :other ['other] #{:cljs})
                  (cljs/step-compile-modules)
                  ;;(cljs/flush-unoptimized)
                  (cljs/closure-optimize)
                  (cljs/flush-modules-to-disk)
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
                       (cljs/step-find-resources "cljs-data/dummy/src")
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
             :cache-dir (io/file "cljs-data/foreign/out/cljs-cache")
             :public-dir (io/file "cljs-data/foreign/out")
             :public-path "out")
      (cljs/step-find-resources-in-jars)
      (cljs/step-find-resources "cljs-data/foreign/src")
      (cljs/add-foreign "jquery.js"
                        '#{jquery}
                        #{}
                        (slurp (io/file "cljs-data/foreign/lib/jquery-2.1.3.min.js"))
                        (slurp (io/file "cljs-data/foreign/lib/jquery.externs.js")))
      (cljs/step-finalize-config)
      (cljs/step-compile-core)
      (cljs/step-configure-module :test ['wants-jquery] #{})
      (cljs/step-compile-modules)
      ;; (cljs/closure-optimize)
      ;; (cljs/flush-modules-to-disk)
      (cljs/flush-unoptimized)
      ))

(deftest test-dummy
  (let [s (-> (cljs/init-state)
              (assoc :optimizations :none
                     :pretty-print true
                     :work-dir (io/file "target/test-cljs-work")
                     :public-dir (io/file "target/test-cljs")
                     :public-path "target/test-cljs")
              (cljs/step-find-resources-in-jars)
              (cljs/step-find-resources "cljs-data/dummy/src")
              (cljs/step-finalize-config)
              (cljs/step-configure-module :test ['shadow.dummy] #{})
              (cljs/step-compile-modules)
              ;; (cljs/closure-optimize)
              ;; (cljs/flush-modules-to-disk)
              ;;(cljs/flush-unoptimized)
              )]
    (println (get-in s [:sources "shadow/dummy.cljs" :output]))))

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


(def ns-tests
  "taken from https://github.com/clojure/clojurescript/blob/master/test/clj/cljs/analyzer_tests.clj"
  ['(ns foo.bar
      (:require {:foo :bar}))
   "Only [lib.ns & options] and lib.ns specs supported in :require / :require-macros"
   '(ns foo.bar
      (:require [:foo :bar]))
   "Library name must be specified as a symbol in :require / :require-macros"
   '(ns foo.bar
      (:require [baz.woz :as woz :refer [] :plop]))
   "Only :as alias and :refer (names) options supported in :require"
   '(ns foo.bar
      (:require [baz.woz :as woz :refer [] :plop true]))
   "Only :as and :refer options supported in :require / :require-macros"
   '(ns foo.bar
      (:require [baz.woz :as woz :refer [] :as boz :refer []]))
   "Each of :as and :refer options may only be specified once in :require / :require-macros"
   '(ns foo.bar
      (:refer-clojure :refer []))
   "Only [:refer-clojure :exclude (names)] form supported"
   '(ns foo.bar
      (:use [baz.woz :exclude []]))
   "Only [lib.ns :only (names)] specs supported in :use / :use-macros"
   '(ns foo.bar
      (:require [baz.woz :as []]))
   ":as must be followed by a symbol in :require / :require-macros"
   '(ns foo.bar
      (:require [baz.woz :as woz]
                [noz.goz :as woz]))
   ":as alias must be unique"
   '(ns foo.bar
      (:unless []))
   "Only :refer-clojure, :require, :require-macros, :use and :use-macros libspecs supported"
   '(ns foo.bar
      (:require baz.woz)
      (:require noz.goz))
   "Only one "
   ])

(def ns-env
  (assoc-in (a/empty-env) [:ns :name] 'cljs.user))

(defn cljs-parse-ns [ns-env form]
  (binding [a/*cljs-ns* 'cljs.user
            a/*analyze-deps* false
            a/*load-macros* false]
    (a/analyze ns-env form)))

(deftest test-parse-ns
  (let [test '(ns something
                "doc before meta"
                {:some :meta}
                (:refer-clojure :exclude (whatever))
                (:use-macros [macro-use :only (that-one)])
                (:require-macros [macro-ns :as m :refer (a-macro)])
                (:require only-symbol
                          [some.ns :as alias :refer (foo) :refer-macros (a-macro-from-some-ns)]
                          [another.ns :as x :include-macros true]
                          :reload-all)
                (:import [goog.ui SomeElement OtherElement]
                         a.fully-qualified.Name))

        a (util/parse-ns test)
        b (cljs-parse-ns ns-env test)]

    (is (= (:name a) (:name b)))
    (is (= (:requires a) (:requires b)))
    (is (= (:require-macros a) (:require-macros b)))
    (is (= (:uses a) (:uses b)))
    (is (= (:use-macros a) (:use-macros b)))
    (is (= (:imports a) (:imports b)))
    (comment
      ;; cljs actually drops the docstring if separate from meta
      (is (= (meta (:name a))
             (meta (:name b))))))


  (is (thrown-with-msg?
        Exception
        #"Only one "
        (util/parse-ns
          '(ns foo.bar
             (:require foo)
             (:require bar)))
        ))

  (comment
    ;; FIXME: lazy, fix the errors messages
    (is (thrown-with-msg?
          Exception
          #"Only \[lib.ns & options\] and lib.ns specs supported in :require / :require-macros"
          (parse-ns
            ns-env
            '(ns foo.bar
               (:require {:foo :bar})))
          ))

    (is (thrown-with-msg?
          Exception
          #"Library name must be specified as a symbol in :require / :require-macros"
          (parse-ns
            ns-env
            '(ns foo.bar
               (:require [:foo :bar])))
          ))

    (is (thrown-with-msg?
          Exception
          #"Only :as alias and :refer \(names\) options supported in :require"
          (parse-ns
            ns-env
            '(ns foo.bar
               (:require [baz.woz :as woz :refer [] :plop])))
          ))

    (is (thrown-with-msg?
          Exception
          #"Only :as and :refer options supported in :require / :require-macros"
          (parse-ns
            ns-env
            '(ns foo.bar
               (:require [baz.woz :as woz :refer [] :plop true])))
          ))


    (is (thrown-with-msg?
          Exception
          #"Each of :as and :refer options may only be specified once in :require / :require-macros"
          (parse-ns
            ns-env
            '(ns foo.bar
               (:require [baz.woz :as woz :refer [] :as boz :refer []])))
          ))

    (is (thrown-with-msg?
          Exception
          #"Only \[:refer-clojure :exclude \(names\)\] form supported"
          (parse-ns
            ns-env
            '(ns foo.bar
               (:refer-clojure :refer [])))
          ))

    (is (thrown-with-msg?
          Exception
          #"Only \[lib.ns :only \(names\)\] specs supported in :use / :use-macros"
          (parse-ns
            ns-env
            '(ns foo.bar
               (:use [baz.woz :exclude []])))
          ))

    (is (thrown-with-msg?
          Exception
          #":as must be followed by a symbol in :require / :require-macros"
          (parse-ns
            ns-env
            '(ns foo.bar
               (:require [baz.woz :as []])))
          ))

    (is (thrown-with-msg?
          Exception
          #":as alias must be unique"
          (parse-ns
            ns-env
            '(ns foo.bar
               (:require [baz.woz :as woz]
                         [noz.goz :as woz])))
          ))
    (is (thrown-with-msg?
          Exception
          #"Only :refer-clojure, :require, :require-macros, :use and :use-macros libspecs supported"
          (parse-ns
            ns-env
            '(ns foo.bar
               (:unless [])))
          ))))

(deftest test-caching-rountrip
  (let [out (File. "target/dummy.cache")
        data {:dummy "data"
              :url (URL. "http://github.com")}
        read (do (cljs/write-cache out data)
                 (cljs/read-cache out))]
    (is (= data read))))
