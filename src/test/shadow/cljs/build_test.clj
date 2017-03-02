(ns shadow.cljs.build-test
  (:use clojure.test
        shadow.fix-test)
  (:require [shadow.cljs.build :as cljs]
            [shadow.cljs.repl :as repl]
            [shadow.cljs.node :as node]
            [shadow.cljs.util :as util]
            [shadow.cljs.umd :as umd]
            [cljs.analyzer :as ana]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.repl :refer (pst)]
            [cljs.analyzer :as a]
            [clojure.set :as set]
            [cljs.closure :as closure]
            [cljs.analyzer.api :as ana-api])
  (:import (java.util.regex Pattern)
           (java.io File ByteArrayInputStream)
           (java.net URL)
           (com.google.javascript.jscomp ClosureCodingConvention CompilerOptions SourceFile)
           (clojure.lang ExceptionInfo)))

(deftest test-initial-scan
  (.setLastModified (io/file "dev/shadow/test_macro.clj") 0)
  (let [state (-> (cljs/init-state)
                  (cljs/find-resources-in-classpath)
                  (cljs/find-resources "test-data")
                  (cljs/finalize-config))]
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
                  (cljs/find-resources-in-classpath)
                  (cljs/find-resources "cljs-data/dummy/src")
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
                  (cljs/finalize-config)
                  (cljs/configure-module :loader ['goog.module.ModuleManager] #{})
                  ;; (cljs/step-configure-module :cljs ['cljs.core] #{:loader})
                  ;; (cljs/step-configure-module :basic ['basic] #{:cljs})
                  ;; (cljs/step-configure-module :other ['other] #{:cljs})
                  (cljs/compile-modules)
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
                    (cljs/find-resources-in-classpath)
                    (cljs/find-resources "target/reload-test")
                    (assoc :optimizations :whitespace
                           :pretty-print true
                           :work-dir (io/file "target/cljs-work")
                           :public-dir (io/file "target/cljs")
                           :public-path "target/cljs")
                    (cljs/finalize-config)
                    (cljs/configure-module :test
                      ['test-a
                       'shadow.ns-on-classpath]
                      #{}))]

      (is (nil? (get-in state [:sources "test_b.cljs"])))

      (cljs/compile-modules state) ;; no error is good enough for now


      ;; wait for a bit
      ;; otherwise the spit may end up in the same millisec as the previous one
      ;; which wait-and-reload can't detect
      (Thread/sleep 50)

      (prn [:before (.lastModified file-a)])
      ;; now we modify it to depend on test-b
      (spit file-a (str/join "\n" ["(ns test-a (:require [test-b]))"
                                   foo-fn]))


      (Thread/sleep 50)
      (.setLastModified file-a (System/currentTimeMillis))
      ;; FIXME: last modified somehow broken ... same as before ... sometimes ...
      (prn [:after (.lastModified (io/file "target/reload-test/test_a.cljs"))])

      (.setLastModified (io/file "src/dev/shadow/ns_on_classpath.cljs") (System/currentTimeMillis))

      (let [modified (cljs/scan-for-modified-files state)
            new (cljs/scan-for-new-files state)]

        (prn [:modified (map :url modified)])
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
            (cljs/compile-modules state)
            ))))))



(deftest test-caching
  (let [do-build
        (fn []
          (-> (cljs/init-state)
              (cljs/enable-source-maps)
              (assoc :optimizations :none
                     :pretty-print false
                     :work-dir (io/file "target/cljs-work")
                     :cache-dir (io/file "target/cljs-cache")
                     :public-dir (io/file "target/cljs")
                     :public-path "target/cljs")
              (cljs/find-resources-in-classpath)
              (cljs/find-resources "cljs-data/dummy/src")
              (cljs/finalize-config)
              (cljs/configure-module :basic ['basic] #{})
              (cljs/compile-modules)
              (cljs/flush-unoptimized)))]
    (println "--- ROUND 1")
    (.setLastModified (io/file "cljs-data/dummy/src/common.cljs") 1)
    (do-build)
    ;; all files should be cached now

    (println "--- ROUND 2")
    ;; should load only cached
    (do-build)

    (println "--- ROUND 3")
    ;; touch one file which should cause a recompile of all dependents too
    (.setLastModified (io/file "cljs-data/dummy/src/common.cljs") (System/currentTimeMillis))
    (do-build)

    ;; FIXME: checkout output that basic and common were recompiled, not just common
    ))


(deftest test-caching-with-jars
  (let [do-build
        (fn [jar-path]
          (-> (cljs/init-state)
              (cljs/enable-source-maps)
              (assoc :optimizations :none
                     :pretty-print false
                     :work-dir (io/file "target/cljs-work")
                     :cache-dir (io/file "target/cljs-cache")
                     :public-dir (io/file "target/cljs")
                     :public-path "target/cljs")
              (cljs/find-resources-in-classpath)
              (cljs/find-resources "cljs-data/dummy/src")
              (cljs/find-resources jar-path)
              (cljs/finalize-config)
              (cljs/configure-module :basic ['basic
                                             'hello-world] #{})
              (cljs/compile-modules)
              (cljs/flush-unoptimized)))]

    (println "--- ROUND 1")
    (do-build "cljs-data/dummy/lib/hello-world-v1.jar")
    ;; all files should be cached now

    (println "--- ROUND 2")
    ;; should load only cached
    (do-build "cljs-data/dummy/lib/hello-world-v1.jar")

    (println "--- ROUND 3")
    ;; v2 is older than our previous build, but must still trigger a recompile
    (do-build "cljs-data/dummy/lib/hello-world-v2.jar")
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
      (cljs/find-resources-in-classpath)
      (cljs/find-resources "cljs-data/foreign/src")
      (cljs/add-foreign "jquery.js"
        '#{jquery}
        #{}
        (slurp (io/file "cljs-data/foreign/lib/jquery-2.1.3.min.js"))
        (slurp (io/file "cljs-data/foreign/lib/jquery.externs.js")))
      (cljs/finalize-config)
      (cljs/configure-module :test ['wants-jquery] #{})
      (cljs/compile-modules)
      ;; (cljs/closure-optimize)
      ;; (cljs/flush-modules-to-disk)
      (cljs/flush-unoptimized)
      ))

(deftest test-dummy
  (let [s (-> (cljs/init-state)
              (assoc :optimizations :none
                     :pretty-print true
                     :cache-level :jars
                     :work-dir (io/file "target/test-cljs-work")
                     :public-dir (io/file "target/test-cljs")
                     :public-path "target/test-cljs")
              (cljs/find-resources-in-classpath)
              (cljs/find-resources "cljs-data/dummy/src")
              (cljs/finalize-config)
              (cljs/configure-module :test ['cljs.repl] #{})
              (cljs/compile-modules)
              ;; (cljs/closure-optimize)
              ;; (cljs/flush-modules-to-disk)
              (cljs/flush-unoptimized)
              )]
    ;; (println (get-in s [:sources "cljs/repl.cljs" :output]))
    ))

(deftest test-ns-with-use
  (let [s (-> (cljs/init-state)
              (assoc :optimizations :none
                     :pretty-print true
                     :work-dir (io/file "target/test-cljs-work")
                     :public-dir (io/file "target/test-cljs")
                     :public-path "target/test-cljs")
              (cljs/find-resources-in-classpath)
              (cljs/find-resources "cljs-data/dummy/src")
              (cljs/configure-module :test ['with-use] #{})
              (cljs/finalize-config)
              (cljs/compile-modules)
              ;; (cljs/closure-optimize)
              ;; (cljs/flush-modules-to-disk)
              ;;(cljs/flush-unoptimized)
              )]
    (println (get-in s [:sources "with_use.cljs" :output]))))

(deftest test-ns-with-rename
  (let [s (-> (cljs/init-state)
              (assoc :optimizations :advanced
                     :pretty-print true
                     :cache-level :jars
                     :work-dir (io/file "target/test-cljs-work")
                     :public-dir (io/file "target/test-cljs")
                     :public-path "target/test-cljs")
              (cljs/find-resources-in-classpath)
              (cljs/find-resources "cljs-data/dummy/src")
              (cljs/configure-module :test ['with-rename] #{})
              (cljs/finalize-config)
              (cljs/compile-modules)
              ;; (cljs/closure-optimize)
              ;; (cljs/flush-modules-to-disk)
              (cljs/flush-unoptimized)
              )]

    (println (get-in s [:sources "with_rename.cljs" :output]))
    ))

(deftest test-flush-compact
  (let [state
        (-> (cljs/init-state)
            (cljs/enable-source-maps)
            (assoc :optimizations :none
                   :pretty-print true
                   :work-dir (io/file "target/test-cljs-work")
                   :public-dir (io/file "target/test-cljs")
                   :public-path "target/test-cljs")
            (cljs/find-resources-in-classpath)
            (cljs/find-resources "cljs-data/dummy/src")
            (cljs/finalize-config)
            (cljs/configure-module :test ['basic] #{})
            (cljs/compile-modules)
            ;; (cljs/closure-optimize)
            ;; (cljs/flush-modules-to-disk)
            ;;(cljs/flush-unoptimized)
            (cljs/flush-unoptimized-compact))]
    (prn :yo)
    ))

(deftest test-bad-files
  (let [state
        (-> (cljs/init-state)
            (cljs/find-resources-in-classpath)
            ;; this should not fail, although there are broken files
            (cljs/find-resources "cljs-data/bad/src")
            (cljs/finalize-config)
            (cljs/configure-module :test ['bad-ns] #{}))]

    (is (thrown? clojure.lang.ExceptionInfo (cljs/compile-modules state)))
    ))

(deftest test-bad-jar
  (let [s (-> (cljs/init-state)
              (assoc :optimizations :none
                     :pretty-print true
                     :work-dir (io/file "target/test-cljs-work")
                     :public-dir (io/file "target/test-cljs")
                     :public-path "target/test-cljs")
              (cljs/find-resources "/Users/zilence/.m2/repository/org/omcljs/om/1.0.0-alpha12/om-1.0.0-alpha12.jar")
              (cljs/find-resources "cljs-data/dummy/src")
              (cljs/finalize-config)
              )]
    (println (get-in s [:sources "cljs/repl.cljc" :output]))))

(deftest test-macro-reloading
  (let [s (-> (cljs/init-state)
              (assoc :optimizations :none
                     :pretty-print true
                     :work-dir (io/file "target/test-cljs-work")
                     :public-dir (io/file "target/test-cljs")
                     :public-path "target/test-cljs")
              (cljs/find-resources-in-classpath)
              (cljs/find-resources "cljs-data/dummy/src")
              (cljs/finalize-config)
              (cljs/configure-module :test ['basic] #{})
              ;; (cljs/compile-modules)
              ;; (cljs/closure-optimize)
              ;; (cljs/flush-modules-to-disk)
              ;;(cljs/flush-unoptimized)
              )]

    (pprint (cljs/find-resources-using-macro s 'shadow.test-macro))
    ))

(deftest test-find-dependents
  (let [s (-> (cljs/init-state)
              (assoc :optimizations :none
                     :public-dir (io/file "target/test-cljs")
                     :public-path "target/test-cljs")
              (cljs/find-resources-in-classpath)
              (cljs/find-resources "cljs-data/dummy/src")
              (cljs/finalize-config)
              (cljs/configure-module :test ['basic] #{})
              ;; (cljs/compile-modules)
              ;; (cljs/closure-optimize)
              ;; (cljs/flush-modules-to-disk)
              ;;(cljs/flush-unoptimized)
              )]

    (pprint (cljs/find-dependent-names s 'common))
    (pprint (cljs/find-dependents-for-names s ["common.cljs"]))

    ))

(deftest test-compiler-error
  (let [s (-> (cljs/init-state)
              (cljs/find-resources-in-classpath)
              (cljs/find-resources "cljs-data/dummy/src")
              (cljs/finalize-config)
              (cljs/configure-module :test ['broken] #{}))]
    (try
      (cljs/compile-modules s)
      (catch Exception e
        (is (instance? ExceptionInfo e))
        (let [data (ex-data e)]
          (is (= :reader-exception (:type data)))
          (is (contains? data :line))
          (is (contains? data :column))
          (is (contains? data :file)))

        ;; (pst e)
        ))
    ))

(deftest test-require-order
  (let [{:keys [require-order] :as ns-ast}
        (util/parse-ns
          '(ns something
             (:use [test.c :only [that]])
             (:import [goog.net XhrIo])
             (:require [test.b :as b]
                       [test.a :as a]
                       [test.d]
                       test.e)))]

    (pprint ns-ast)
    (is (= require-order '[test.c goog.net.XhrIo test.b test.a test.d test.e]))
    ))


(deftest test-excute-affected-tests
  (-> (cljs/init-state)
      (assoc :optimizations :none
             :pretty-print true
             :work-dir (io/file "target/test-cljs-work")
             :cache-dir (io/file "target/test-cljs-cache")
             :cache-level :jars
             :public-dir (io/file "target/test-cljs")
             :public-path "target/test-cljs")
      (cljs/find-resources-in-classpath)
      (cljs/find-resources "cljs-data/dummy/src")
      (cljs/find-resources "cljs-data/dummy/test")
      (node/execute-affected-tests! ["basic.cljs"])))


(deftest test-nodejs-build
  (let [state
        (-> (cljs/init-state)
            ;; (cljs/enable-emit-constants)
            (cljs/find-resources-in-classpath)
            (cljs/find-resources "cljs-data/node/src")

            (merge {:cache-dir (io/file "target/test-cljs-cache")
                    :cache-level :jars})

            (node/configure
              {:main 'test.server/main
               :output-to "cljs-data/node/out/my-app.js"})
            (node/compile)
            (node/flush))]

    (println (slurp "cljs-data/node/out/my-app.js"))
    ;; (println (slurp "cljs-data/node/out/src/test/server.js"))

    ))

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
                (:refer-clojure :exclude (whatever) :rename {assoc cossa + plus})
                (:use-macros [macro-use :only (that-one)])
                (:require-macros [macro-ns :as m :refer (a-macro) :rename {a-macro b-macro}]
                                 [something :as that-m])
                (:require only-symbol
                          [some.ns :as alias :refer (foo x) :refer-macros (a-macro-from-some-ns) :rename {foo bar}]
                          [another.ns :as x :include-macros true]
                          :reload-all)
                (:use [something.fancy :only [everything] :rename {everything nothing}])
                (:import [goog.ui SomeElement OtherElement]
                         a.fully-qualified.Name))

        a (util/parse-ns test)
        b (cljs-parse-ns ns-env test)
        ]

    (-> b
        (dissoc :env :form)
        (pprint))

    (is (= (:name a) (:name b)))
    (is (= (:requires a) (:requires b)))
    (is (= (:require-macros a) (:require-macros b)))
    (is (= (:uses a) (or (:uses b) {}))) ;; CLJS has a bug that leaves :uses as nil if the only refered var was renamed
    (is (= (:use-macros a) (:use-macros b)))
    (is (= (:imports a) (:imports b)))
    (is (= (:renames a) (:renames b)))
    (is (= (:rename-macros a) (:rename-macros b)))
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


(comment
  ;; node.js hurts my head .. how is this node_modules/thing/node_modules/other/node_modules/thing/node_modules shit ok?
  (defn get-module-roots []
    (->> (file-seq (io/file "cljs-data/commonjs/node_modules"))
         (filter #(.isDirectory %))
         (filter #(re-find #"node_modules/[^/]+$" (.getAbsolutePath %)))
         (map #(.getAbsolutePath %))
         (distinct)
         (into [])
         ))

  (deftest test-es6-conversion
    (let [cc (cljs/make-closure-compiler)
          co (doto (CompilerOptions.)
               (.setPrettyPrint true)
               (.setModuleRoots (get-module-roots))
               (.setCodingConvention (ClosureCodingConvention.))
               (.setParseJsDocDocumentation false)
               (.setProcessCommonJSModules true))

          file (io/file "cljs-data/commonjs/node_modules/react")
          files (->> (file-seq file)
                     (filter #(.isFile %))
                     (filter #(.endsWith (.getName %) ".js"))
                     (remove #(.contains (.getAbsolutePath %) "/test"))
                     (remove #(.contains (.getAbsolutePath %) "__tests__"))
                     (map #(SourceFile/fromFile %))
                     (into []))]
      (.compile cc [] files co)


      ;; (pprint (.getInputsById cc))

      (spit "tmp/wahnsinn.js" (.toSource cc))
      )))

(deftest test-ignore-patterns
  (let [state (cljs/init-state)]
    (is (cljs/should-ignore-resource? state "node_modules/react/addons.js"))
    (is (not (cljs/should-ignore-resource? state "shadow/dom.js")))
    ))


(defn basic-repl-setup []
  (-> (cljs/init-state)
      (cljs/enable-source-maps)
      (assoc :optimizations :none
             :pretty-print true
             :cache-level :jars
             :public-dir (io/file "cljs-data/dummy/out")
             :public-path "out")
      (cljs/find-resources-in-classpath)
      (cljs/find-resources "cljs-data/dummy/src")
      (cljs/find-resources "cljs-data/dummy/test")

      (cljs/finalize-config)
      (cljs/configure-module :cljs ['cljs.core] #{})
      (repl/prepare)))


(deftest test-plain-js
  (let [{:keys [repl-state] :as s}
        (-> (basic-repl-setup)
            ;; (repl/process-input "(js/console.log \"yo\")")
            (repl/process-input "(def x 1)")
            (repl/process-input "x")
            (repl/process-input "js/test"))
        action (get-in s [:repl-state :repl-actions 0])]
    (pprint action)))

(deftest test-repl-dump
  (let [{:keys [repl-state] :as s}
        (-> (basic-repl-setup)
            (repl/process-input "(repl-dump)"))]
    (pprint repl-state)))

(deftest test-basic-require
  (let [{:keys [repl-state] :as s}
        (-> (basic-repl-setup)
            (repl/process-input "(require 'basic)"))
        action (get-in s [:repl-state :repl-actions 0])]
    (pprint repl-state)
    (pprint action)))


(deftest test-basic-def
  (let [{:keys [repl-state] :as s}
        (-> (basic-repl-setup)
            (repl/process-input "(def x 1)")
            (repl/process-input "(inc x)")
            (repl/process-input "x"))]

    (pprint repl-state)
    ))

(deftest test-repl-if
  (let [{:keys [repl-state] :as s}
        (-> (basic-repl-setup)
            (repl/process-input "(if (= 1 2) 1 2)"))]

    (pprint repl-state)
    ))

(deftest test-basic-require-and-call
  (let [{:keys [repl-state] :as s}
        (-> (basic-repl-setup)
            (repl/process-input "(require '[clojure.string :as str])")
            (repl/process-input "(str/trim \"hello world \")"))]

    (pprint repl-state)
    ))

(deftest test-require-with-reload
  (let [s (-> (basic-repl-setup)
              (repl/process-input "(require ['basic :as 'something] :reload-all)"))
        action (get-in s [:repl-state :repl-actions 0])]
    (pprint (:repl-state s))
    (pprint action)))

(deftest test-basic-require-with-macro
  (let [{:keys [repl-state] :as s}
        (-> (basic-repl-setup)
            (repl/process-input "(require '[shadow.test-macro :as tm])")
            ;; this is a macro that should emit (prn ...) not call shadow.test_macro.hello()
            (repl/process-input "(tm/hello)"))]

    (pprint (:repl-actions repl-state))
    ))

(deftest test-in-ns
  (let [{:keys [repl-state] :as state}
        (-> (basic-repl-setup)
            (repl/process-input "(in-ns 'basic)"))

        {:keys [repl-actions]} repl-state]

    (is (= 1 (count repl-actions)))
    (is (= :repl/set-ns (-> repl-actions first :type)))
    (is (= 'basic (-> repl-actions first :ns)))
    ))


(deftest test-load-file
  (let [abs-path (.getAbsolutePath (io/file "cljs-data" "dummy" "src" "basic.cljs"))
        {:keys [repl-state] :as state}
        (-> (basic-repl-setup)
            (repl/process-input (str "(load-file \"" abs-path "\")")))

        {:keys [repl-actions]} repl-state]

    (prn repl-state)
    (pprint repl-actions)
    ))

(deftest test-alias-constants
  (let [state
        (-> (cljs/init-state)
            (cljs/merge-compiler-options
              {:optimizations :advanced
               :pretty-print true})
            (cljs/merge-build-options
              {:public-dir (io/file "cljs-data/dummy/out")
               :public-path "out"})
            ;; (cljs/enable-emit-constants)
            (cljs/find-resources-in-classpath)
            (cljs/find-resources "cljs-data/dummy/src")
            (cljs/configure-module :cljs '[cljs.core] #{})
            (cljs/configure-module :basic ['basic] #{:cljs})
            (cljs/configure-module :other ['other] #{:cljs})
            (cljs/compile-modules)
            (cljs/closure-optimize)
            (cljs/flush-modules-to-disk)
            ;; (cljs/flush-unoptimized)
            )]

    (println (-> state :optimized (nth 2) :output))
    (prn [:done])
    ))


(deftest test-umd-generator
  (let [state
        (-> (cljs/init-state)
            (cljs/find-resources-in-classpath)
            (cljs/find-resources "cljs-data/dummy/src")
            (umd/create-module
              {:test 'basic/hello}
              {:output-to "target/umd/dummy.js"})
            (cljs/compile-modules)
            (cljs/closure-optimize :advanced)
            (umd/flush-module))]
    :done
    ))

(deftest test-elide-asserts
  (let [state
        (-> (cljs/init-state)
            (cljs/find-resources-in-classpath)
            (cljs/find-resources "cljs-data/dummy/src")
            (cljs/prepare-compile)
            (cljs/merge-build-options
              {:public-path "target/assert-out"
               :public-dir (io/file "target/assert-out")})
            (cljs/merge-compiler-options
              {:pretty-print true
               :pseudo-names true
               :closure-defines {"cljs.core._STAR_assert_STAR_" false}})
            (cljs/configure-module :cljs '[cljs.core] #{})
            (cljs/configure-module :test '[with-asserts] #{:cljs})
            (cljs/compile-modules)
            (cljs/closure-optimize :advanced)
            (cljs/flush-modules-to-disk))]
    (println (-> state :optimized second :output))

    ))


(deftest trying-to-break-static-fns
  (let [{:keys [repl-state] :as state}
        (-> (basic-repl-setup)
            (repl/process-input "(def x (fn [a b] (+ a b)))")
            (repl/process-input "(def y (fn [] (x 1 2)))")
            (repl/process-input "(def x (reify IFn (-invoke [_ a b] (- a b))))")
            (repl/process-input "(binding [x (fn [& args] (apply - args))] (y))")
            (repl/process-input "(y)")
            )]
    (->> repl-state
         :repl-actions
         (map :js)
         (map println)
         (doall)
         )))

(deftest run-all-tests-breakage
  (let [state
        (-> (basic-repl-setup)
            (repl/process-input "(require 'cljs.test)")
            (repl/process-input "(cljs.test/run-all-tests)"))]

    ))


(deftest test-repl-load-file
  (let [abs-file
        (-> (io/file "cljs-data/dummy/src/basic.cljs")
            (.getAbsolutePath))

        {:keys [repl-state] :as state}
        (-> (basic-repl-setup)
            (repl/process-input (str "(load-file \"" abs-file "\")")))]

    (pprint repl-state)
    ))

(deftest test-repl-require
  (let [{:keys [repl-state] :as state}
        (basic-repl-setup)]

    (pprint (:current repl-state))

    (let [{:keys [repl-state] :as state}
          (-> state
              (repl/process-input "(require 'clojure.string)"))]

      (pprint (:current repl-state)))
    ))

(deftest test-repl-source-map
  (let [{:keys [repl-state] :as state}
        (-> (basic-repl-setup)
            (repl/prepare)
            (repl/process-input "(let [x 1] (throw (js/Error. \"foo\")))"))]

    ;; (pprint repl-state)
    ))

(deftest test-auto-alias-clojure-to-cljs
  (let [state
        (-> (cljs/init-state)
            (assoc :cache-level :jars)
            (cljs/find-resources-in-classpath)
            (cljs/find-resources "cljs-data/auto-alias")
            (cljs/prepare-compile)
            (cljs/merge-build-options
              {:public-path "target/auto-alias-out"
               :public-dir (io/file "target/auto-alias-out")})
            (cljs/configure-module :test '[test.alias] #{})
            (cljs/compile-modules))

        output
        (get-in state [:sources "test/alias.cljs" :output])]

    (is (not (str/includes? output "clojure.pprint")))
    (println output)
    ))

(deftest test-repl-stream
  (let [in
        (ByteArrayInputStream. (.getBytes "(def foo 1)"))

        {:keys [compiler-env repl-state] :as state}
        (-> (cljs/init-state)
            (cljs/find-resources-in-classpath)
            (repl/prepare)
            (repl/process-input-stream in)
            )]

    (pprint (:repl-actions repl-state))))