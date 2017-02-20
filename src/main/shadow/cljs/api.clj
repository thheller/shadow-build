(ns shadow.cljs.api)

(defn build-dev
  {:doc "build the project, wait for file changes, repeat"
   :deprecated true}
  [{:keys [live-reload source-paths public-dir public-path modules test] :as config} & args]
  (throw (ex-info "sorry this function is gone" {}))
  )

(defn build-prod
  {:doc "build the project with advanced optimizations"
   :deprecated true}
  [{:keys [source-paths public-dir public-path modules] :as config} & args]
  (throw (ex-info "sorry this function is gone" {}))
  )


