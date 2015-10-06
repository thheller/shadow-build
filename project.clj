(defproject thheller/shadow-build "1.0.0-alpha16"
  :description "cljs compiler"
  :url "https://github.com/thheller/shadow-build"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[aysylu/loom "0.5.0"]
                 [http-kit "2.1.19"]
                 [com.cognitect/transit-clj "0.8.275"]
                 ;; [org.clojure/core.typed "0.2.84"]
                 ]


  :source-paths ["src/clj"
                 "src/cljs"]

  :java-source-paths ["src/java"]

  :profiles {:dev {:source-paths ["src/dev"]
                   :dependencies [[org.clojure/clojure "1.7.0"]
                                  [org.clojure/tools.namespace "0.2.4"]
                                  [org.clojure/clojurescript "1.7.48"]
                                  [junit/junit "4.12"]
                                  [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                                  ;; [thheller/clojurescript "0.0-2665+948"]
                                  ]}}

  :plugins [[codox "0.8.13"]]
  
  :codox {:sources ["src/clj"]
          :exclude [shadow.cljs.util]
          :output-dir "doc/codox"}

  ;; this stuff is deprecated
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
