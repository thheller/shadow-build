(defproject thheller/shadow-build "1.0.0-alpha1"
  :description "cljs compiler"
  :url "https://github.com/thheller/shadow-build"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[aysylu/loom "0.5.0"]]
  

  :source-paths ["src/clj"]

  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/clojure "1.6.0"]
                                  [org.clojure/tools.namespace "0.2.4"]
                                  [org.clojure/clojurescript "0.0-2322"]
                                  ]}} 
  
  :aliases {"cljs-dev" ["run" "-m" "shadow.cljs.api/build-dev" :project/cljs]
            "cljs-prod" ["run" "-m" "shadow.cljs.api/build-prod" :project/cljs]}
  
  :cljs {:modules [{:name :cljs
                    :main 'cljs.core}
                   {:name :main
                    :main 'basic
                    :depends-on #{:cljs}}]

         :source-paths ["test-data"]
         :public-dir "target/cljs"
         :public-path "target/cljs"}
  )
