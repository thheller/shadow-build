(defproject thheller/shadow-build "1.0.239"
  :description "cljs compiler"
  :url "https://github.com/thheller/shadow-build"
  :license
  {:name "Eclipse Public License"
   :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies
  [[org.clojure/clojure "1.9.0-alpha14"]
   [org.clojure/clojurescript "1.9.293"]
   [aysylu/loom "0.6.0"]
   [com.cognitect/transit-clj "0.8.295"]]

  :source-paths
  ["src/clj"
   "src/cljs"]

  :java-source-paths
  ["src/java"]

  :profiles
  {:dev
   {:source-paths
    ["src/dev"]
    :dependencies
    [[org.clojure/tools.namespace "0.2.11"]
     [junit/junit "4.12"]
     ]}}

  :plugins
  [[codox "0.8.13"]]

  :codox
  {:sources ["src/clj"]
   :exclude [shadow.cljs.util]
   :output-dir "doc/codox"})
