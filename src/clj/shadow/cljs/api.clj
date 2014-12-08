(ns shadow.cljs.api
  (:import (java.util UUID))
  (:require [shadow.cljs.build :as cljs]
            [clojure.java.io :as io]
            [clojure.pprint :refer (pprint)]
            [org.httpkit.server :as hk]
            [cljs.compiler :as comp]))


(defn add-source-paths [state source-paths]
  (reduce cljs/step-find-resources state source-paths))

(defn define-modules [state modules]
  (reduce (fn [state {:keys [main name depends-on prepend] :as args}]
            (cljs/step-configure-module
              state
              name
              (cond
                (symbol? main)
                [main]

                :default
                main)
              (or depends-on #{})
              (-> args
                  (cond->
                    prepend
                    (assoc :prepend (slurp (io/resource prepend)))))))
          state
          modules))

(defn start-live-reload-server [{:keys [logger] :as state} {:keys [port host] :as config}]
  (let [changes (atom {})
        handler (fn [ring-request]
                  (let [client-id (UUID/randomUUID)]
                    (hk/with-channel
                      ring-request channel
                      (if (hk/websocket? channel)
                        (do (add-watch changes client-id
                                       (fn [_ _ _ new]
                                         (hk/send! channel (pr-str new))))

                            (doto channel
                              (hk/on-receive (fn [data]
                                               (prn [:websocket-sending-me-things data])))

                              (hk/on-close (fn [status]
                                             (println (format "Closing WebSocket: %s [%s]" client-id status))
                                             (remove-watch changes client-id)))))

                        (hk/send! channel {:status 200
                                           :headers {"Content-Type" "text/plain"}
                                           :body "Long polling?"})))))]

    (add-watch changes :change-dump
               (fn [_ _ _ {:keys [js] :as new}]
                 (doseq [{:keys [name]} js]
                   (cljs/log-progress logger (format "RELOAD: %s" name)))))


    (let [host (or host "localhost")
          instance (hk/run-server handler {:ip host
                                           :port (or port 0)})]
      {:instance instance
       :port (:local-port (meta instance))
       :host host
       :changes changes
       })))

(defn setup-live-reload [{:keys [public-path] :as state} {:keys [before-load after-load] :as config}]
  (if (not config)
    state
    (let [{:keys [host port] :as server} (start-live-reload-server state config)
          config (assoc config
                        :socket-url (str "ws://" host ":" port "/socket")
                        :public-path public-path
                        :before-load (when before-load
                                       (str (comp/munge before-load)))
                        :after-load (when after-load
                                      (str (comp/munge after-load))))]
      (println (format "Live-Reload started: %s" (pr-str config)))
      (-> state
          (assoc :live-reload {:server server
                               :config config})
          ;; (cljs/step-find-resources "src/cljs") ;; FIXME: will be in JAR!
          (update-in [:modules (:default-module state) :mains] conj 'shadow.cljs.live-reload)
          ;; now if this isn't a fine hack
          ;; we ship with a live-reload ns and append stuff to it
          ;; so we can configure it without requiring the user to do it manually
          ;; can't do it in module :append-js cause of weird goog.require behavior and execution ordering
          ;; shadow.cljs.live_reload does not exist immediately after goog.require('shadow.cljs.live_reload')
          ;; only after the file with the require finishes loading.
          (update-in [:sources "shadow/cljs/live_reload.cljs" :source] str "\n(setup " (pr-str config) ")\n")
          ))))

(defn notify-live-reload [{:keys [live-reload] :as state} modified]
  (when (and live-reload (seq modified))
    (let [data (->> modified
                    (map (fn [{:keys [name js-name provides]}]
                           {:name name
                            :js-name js-name
                            :provides (map #(str (comp/munge %)) provides)}))
                    (into []))
          changes (get-in state [:live-reload :server :changes])]
      (swap! changes assoc-in [:js] data)
      ))
  state)

(defn build-dev
  "build the project, wait for file changes, repeat"
  [{:keys [live-reload source-paths public-dir public-path modules] :as config} & args]
  (when (nil? config)
    (throw (ex-info "no cljs config, wrong key?" {})))
  ;; FIXME: validate config!

  (loop [state (-> (cljs/init-state)
                   (merge (dissoc config :modules :source-paths :public-dir :public-path))
                   (cljs/enable-source-maps)
                   (assoc :optimizations :none
                          :pretty-print true
                          :work-dir (io/file "target/cljs-work")
                          :public-dir (io/file public-dir)
                          :public-path public-path)
                   (cljs/step-find-resources-in-jars)
                   (add-source-paths source-paths)
                   (cljs/step-finalize-config)
                   (cljs/step-compile-core)
                   (define-modules modules)
                   (setup-live-reload live-reload))

         modified []]

    (let [state (try
                  (-> state
                      (cljs/step-compile-modules)
                      (cljs/flush-unoptimized)
                      ;; only notify after the build is actually completed!
                      (notify-live-reload modified))
                  (catch Throwable t
                    (println (str "COMPILATION FAILED: " t))
                    (.printStackTrace t)
                    state))
          modified (cljs/wait-for-modified-files! state)]
      (recur (cljs/reload-modified-files! state modified) modified)
      ))

  ;; never really gets here, ctrl+c to stop
  :done)

(defn build-prod
  "build the project with advanced optimizations"
  [{:keys [source-paths public-dir public-path modules] :as config} & args]
  (when (nil? config)
    (throw (ex-info "no cljs config, wrong key?" {})))
  ;; FIXME: validate config!

  (-> (cljs/init-state)
      (merge (dissoc config :modules :source-paths :public-dir :public-path))
      (assoc :optimizations :advanced
             :pretty-print false
             :work-dir (io/file "target/cljs-work-prod")
             :public-dir (io/file public-dir)
             :public-path public-path)
      (cljs/step-find-resources-in-jars)
      (add-source-paths source-paths)
      (cljs/step-finalize-config)
      (cljs/step-compile-core)
      (define-modules modules)
      (cljs/step-compile-modules)
      (cljs/closure-optimize)
      (cljs/flush-modules-to-disk))

  ;; closure compiler thread keeps the jvm alive but all work is done, dunno why this is
  (shutdown-agents)
  (System/exit 0))


