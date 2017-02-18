(ns dummy
  (:require [shadow.cljs.build :as cljs]
            [shadow.cljs.repl :as repl]
            [clojure.pprint :refer (pprint)]
            [cljs.analyzer :as ana]))


(let [{:keys [repl-state compiler-env] :as state}
      (-> (cljs/init-state)
          (cljs/find-resources-in-classpath)
          (repl/prepare)
          (repl/process-input "(def foo 1)"))]


  (-> (get-in compiler-env [::ana/namespaces 'cljs.user :defs])
      (pprint))

  )


