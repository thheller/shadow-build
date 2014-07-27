(defproject thheller/shadow-build "0.7.1"
  :description "cljs compiler"
  :url "https://github.com/thheller/shadow-build"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[aysylu/loom "0.5.0"]
                 [org.clojure/tools.logging "0.3.0"]]

  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/clojure "1.6.0"]
                                  [org.clojure/tools.namespace "0.2.4"]
                                  [org.clojure/clojurescript "0.0-2277"
                                   :exclusions [org.clojure/google-closure-library]]
                                  [org.slf4j/slf4j-log4j12 "1.7.7"]
                                  [log4j "1.2.17"]
                                  ]}} 
  
  :source-paths ["src/clj"])
