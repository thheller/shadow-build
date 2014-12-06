(defproject thheller/shadow-build "1.0.0-alpha1"
  :description "cljs compiler"
  :url "https://github.com/thheller/shadow-build"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[aysylu/loom "0.5.0"]]
  

  :source-paths ["src/clj"
                 "src/cljs"]

  ;; make cursive happy, see https://github.com/cursiveclojure/cursive/issues/665
  :cljsbuild {:builds {:main {:source-paths ["src/cljs"]}}}

  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/clojure "1.6.0"]
                                  [org.clojure/tools.namespace "0.2.4"]
                                  [org.clojure/clojurescript "0.0-2322"]
                                  [http-kit "2.1.18"]]}}

  :aliases {"cljs-dev" ["run" "-m" "shadow.cljs.api/build-dev" :project/cljs]
            "cljs-prod" ["run" "-m" "shadow.cljs.api/build-prod" :project/cljs]}
  
  :cljs {:modules [{:name :cljs
                    :main 'cljs.core}
                   {:name :basic
                    :main 'basic
                    :depends-on #{:cljs}}
                   {:name :other
                    :main 'other
                    :depends-on #{:cljs}
                    }]

         :live-reload {:before-load 'basic.stop-app
                       :after-load 'basic.start-app}
         :source-paths ["test-data"]
         :public-dir "target/cljs"
         :public-path "target/cljs"}
  )
