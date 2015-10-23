(defproject thheller/shadow-build "1.0.139"
  :description "cljs compiler"
  :url "https://github.com/thheller/shadow-build"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.145"]
                 [aysylu/loom "0.5.0"]
                 [com.cognitect/transit-clj "0.8.283"]]

  :source-paths ["src/clj"
                 "src/cljs"]


  :profiles {:dev {:source-paths ["src/dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.11"]
                                  [junit/junit "4.12"]
                                  ]}}

  :plugins [[codox "0.8.13"]]
  
  :codox {:sources ["src/clj"]
          :exclude [shadow.cljs.util]
          :output-dir "doc/codox"})
