(defproject thheller/shadow-build "1.0.0-alpha5"
  :description "cljs compiler"
  :url "https://github.com/thheller/shadow-build"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[aysylu/loom "0.5.0"]
                 [http-kit "2.1.18"]]


  :source-paths ["src/clj"
                 "src/cljs"]

  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/clojure "1.6.0"]
                                  [org.clojure/tools.namespace "0.2.4"]
                                  ;; [org.clojure/clojurescript "0.0-2657"]
                                  [thheller/clojurescript "0.0-2665+948"]
                                  ]}}

  :aliases {"cljs-dev" ["run" "-m" "shadow.cljs.api/build-dev" :project/cljs]
            "cljs-prod" ["run" "-m" "shadow.cljs.api/build-prod" :project/cljs]}

  :cljs {:modules [{:name :cljs
                    :main 'cljs.core}
                   {:name :basic
                    :main 'basic
                    :depends-on #{:cljs}}
                   {:name :other
                    :main 'other
                    :depends-on #{:cljs}}]

         :source-paths ["src/cljs"
                        "cljs-data/dummy/src"]
         :public-dir "target/cljs"
         :public-path "target/cljs"

         :live-reload {;; :host "localhost" ;; optional, defaults to localhost
                       ;; :port 8889 ;; optional, defaults to random open port
                       :before-load 'basic.stop-app
                       :after-load 'basic.start-app
                       }}








  ;; make cursive happy, see https://github.com/cursiveclojure/cursive/issues/665
  ;; shadow-build has nothing to do with lein-cljsbuild!
  :cljsbuild {:builds {:main {:source-paths ["src/cljs" "test-data"]}}}
  )
