(defproject thheller/shadow-build "1.0.127"
  :description "cljs compiler"
  :url "https://github.com/thheller/shadow-build"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[aysylu/loom "0.5.0"]
                 [com.cognitect/transit-clj "0.8.283"]]

  :source-paths ["src/clj"
                 "src/cljs"]


  :profiles {:dev {:source-paths ["src/dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.11"]
                                  [org.clojure/clojurescript "1.7.122"]
                                  [org.clojure/clojure "1.7.0"]
                                  [junit/junit "4.12"]
                                  ]}}

  :plugins [[codox "0.8.13"]]
  
  :codox {:sources ["src/clj"]
          :exclude [shadow.cljs.util]
          :output-dir "doc/codox"}

  ;; make cursive happy, see https://github.com/cursiveclojure/cursive/issues/665
  ;; shadow-build has nothing to do with lein-cljsbuild!
  :cljsbuild {:builds {:main {:source-paths ["src/cljs" "test-data"]}}}
  )
