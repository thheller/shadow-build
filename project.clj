(defproject thheller/shadow-build "1.0.20170429-17"
  :description "cljs compiler"
  :url "https://github.com/thheller/shadow-build"
  :license
  {:name "Eclipse Public License"
   :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies
  [[org.clojure/clojure "1.9.0-alpha15"]
   [org.clojure/clojurescript "1.9.521"]
   [com.cognitect/transit-clj "0.8.300"
    :exclusions
    [org.msgpack/msgpack]]
   [org.clojure/java.classpath "0.2.3"]]

  :source-paths
  ["src/main"]

  :java-source-paths
  ["src/java"]

  :test-paths
  ["src/test"]

  :profiles
  {:dev
   {:source-paths
    ["src/dev"]
    :dependencies
    []}}

  #_[:plugins
     [[codox "0.8.13"]]

     :codox
     {:sources ["src/main"]
      :exclude [shadow.cljs.util]
      :output-dir "doc/codox"}])
