(defproject thheller/shadow-build "0.6.0"
  :description "cljs compiler"
  :url "https://github.com/thheller/shadow-build"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies []

  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/clojure "1.6.0"]
                                  [aysylu/loom "0.5.0"]
                                  [org.clojure/tools.namespace "0.2.4"]
                                  [org.clojure/clojurescript "0.0-2138"
                                   :exclusions [org.clojure/google-closure-library]]]}} 
  
  :source-paths ["src/clj"])
