(ns shadow.cljs.live-reload
  (:import (java.util UUID)
           (java.io InputStreamReader BufferedReader))
  (:require [shadow.cljs.build :as cljs]
            [shadow.cljs.repl :as repl]
            [clojure.java.io :as io]
            [clojure.pprint :refer (pprint)]
            [clojure.data.json :as json]
            [org.httpkit.server :as hk]
            [cljs.compiler :as comp]
            [clojure.edn :as edn]
            [clojure.core.async :as async :refer (go <! >! <!! >!! alt! timeout)]
            [shadow.cljs.repl :as repl]))

(defn handle-client-data [msg]
  (let [{:keys [value] :as msg} (edn/read-string msg)]
    (println value)
    (.format System/err "REPL-RESULT: %s%n" (object-array [(pr-str msg)]))
    ))

(defn- start-server [{:keys [logger] :as state} {:keys [port host] :as config}]
  (let [clients (atom {})
        handler (fn [ring-request]
                  (let [client-id (UUID/randomUUID)]
                    (hk/with-channel
                      ring-request channel
                      (if (hk/websocket? channel)
                        (do (swap! clients assoc client-id channel)

                            (doto channel
                              (hk/on-receive (fn [data]
                                               (handle-client-data data)))

                              (hk/on-close (fn [status]
                                             (println (format "Closing WebSocket: %s [%s]" client-id status))
                                             (swap! clients dissoc client-id))))
                            )

                        (hk/send! channel {:status 406 ;; not-acceptable
                                           :headers {"Content-Type" "text/plain"}
                                           :body "websocket required"})))))]

    (let [host (or host "localhost")
          instance (hk/run-server handler {:ip host
                                           :port (or port 0)})]
      {:instance instance
       :port (:local-port (meta instance))
       :host host
       :clients clients
       :broadcast (fn [type data]
                    (doseq [[client-id client] @clients]
                      (try
                        (hk/send! client (pr-str {:type type
                                                  :data data}))
                        (catch Exception e
                          (prn [:failed-to-broadcast client-id e])))))
       })))

(defn get-css-state [packages]
  (reduce-kv
    (fn [s k {:keys [manifest] :as v}]
      (let [file (io/file manifest)]
        (assoc s k (if (.exists file)
                     (.lastModified file)
                     0))))
    {}
    packages))

(defn- read-css-manifest [{:keys [manifest path] :as package}]
  (->> (io/file manifest)
       (slurp)
       (json/read-str)
       (assoc package :manifest)))

(defn- setup-css-watch [state packages]
  (let [package-names (keys packages)
        broadcast-fn (get-in state [:live-reload :server :broadcast])
        css-watch (doto (Thread.
                          (fn []
                            (loop [css-state (get-css-state packages)]
                              (Thread/sleep 500) ;; FIXME: don't use sleep
                              (let [new-state (get-css-state packages)
                                    changed (reduce
                                              (fn [changed package-name]
                                                (let [old (get css-state package-name)
                                                      new (get new-state package-name)]
                                                  (if (not= old new)
                                                    (conj changed package-name)
                                                    changed)))
                                              #{}
                                              package-names)]
                                (when (seq changed)
                                  (let [change-data (reduce (fn [data package-name]
                                                              (assoc data package-name (read-css-manifest (get packages package-name))))
                                                            {}
                                                            changed)]
                                    (broadcast-fn :css change-data)))
                                (recur new-state)
                                ))))
                    (.start))]
    (assoc-in state [:live-reload :css-watch] css-watch)))

(defn setup
  "configure live-reload, use after cljs/finalize-config

   config is a map with these options:
   :host the interface to create the websocket server on (defaults to \"localhost\")
   :port the port to listen to (defaults to random port)
   :before-load fully qualified function name to execute BEFORE reloading new files
   :after-load fully qualified function name to execute AFTER reloading ALL files

   live-reload will only load namespaces that were already required"
  [state config]
  (let [{:keys [public-path logger]} state
        {:keys [before-load after-load css-packages]} config]
    (if (not config)
      state
      (let [{:keys [host port] :as server} (start-server state config)
            config (assoc config
                     :socket-url (str "ws://" host ":" port "/socket")
                     :public-path public-path
                     :before-load (when before-load
                                    (str (comp/munge before-load)))
                     :after-load (when after-load
                                   (str (comp/munge after-load))))]
        (cljs/log-progress logger (format "Live-Reload started: %s" (pr-str config)))
        (-> state
            (assoc :live-reload {:server server
                                 :config config})
            ;; (cljs/step-find-resources "src/cljs") ;; FIXME: will be in JAR!
            (cljs/merge-resource
              {:type :cljs
               :last-modified (System/currentTimeMillis)
               :input (atom (str "(ns shadow.cljs.live-reload-init (:require [shadow.cljs.live-reload :as lr]))"
                                 "(lr/setup " (pr-str config) ")"))
               :name "shadow/cljs/live_reload_init.cljs"
               :js-name "shadow/cljs/live_reload_init.js"
               :requires #{'shadow.cljs.live-reload}
               :provides #{'shadow.cljs.live-reload-init}
               })
            (update-in [:modules (:default-module state) :mains] conj 'shadow.cljs.live-reload-init)
            (cond->
              css-packages
              (setup-css-watch css-packages))
            )))))

(defn- notify! [{:keys [live-reload] :as state} modified]
  (when (and live-reload (seq modified))
    (let [data (->> modified
                    (map (fn [name]
                           (let [{:keys [js-name provides]} (get-in state [:sources name])]
                             {:name name
                              :js-name js-name
                              :provides (map #(str (comp/munge %)) provides)})))
                    (into []))
          broadcast-fn (get-in state [:live-reload :server :broadcast])]
      (broadcast-fn :js data)))
  state)

(defn wrap
  "wrap a watch-and-repeat! callback which will handle the live-reload notifications"
  [callback]
  (fn [state modified]
    (-> state
        (callback modified)
        (notify! modified))))

(defn setup-repl [state config]
  (repl/prepare state))

(defn handle-repl-input [{:keys [repl-state] :as state} repl-input]
  (prn [:handle-repl-input repl-input])

  (let [clients @(get-in state [:live-reload :server :clients])]

    (cond
      (> (count clients) 1)
      (do (prn [:too-many-clients (count clients)])
          state)

      (zero? (count clients))
      (do (prn [:no-browser-connected])
          state)

      :else
      (let [[client-id channel] (first clients)
            start-idx (count (:repl-actions repl-state))

            {:keys [repl-state] :as state}
            (try
              (repl/process-input state repl-input)
              (catch Throwable e
                (prn [:failed-to-process-repl-input e])
                (pprint repl-state)
                state
                ))

            new-actions (subvec (:repl-actions repl-state) start-idx)]

        (doseq [[idx action] (map-indexed vector new-actions)
                :let [idx (+ idx start-idx)
                      action (assoc action :id idx)]]

          (prn [:invoke action])
          (hk/send! channel (pr-str action)))

        state
        ))))

(defn start-repl [state config callback]
  (let [repl-input (async/chan)
        state (-> state
                  (setup config)
                  (setup-repl config)
                  (callback []))]

    (go (loop [state state
               i 0]
          (alt!
            repl-input
            ([v]
              (when-not (nil? v)
                (recur (handle-repl-input state v) i)))

            (timeout 500)
            ([_]
              (let [modified (cljs/scan-for-modified-files state)
                    modified (if (zero? (mod i 5))
                               (concat modified (cljs/scan-for-new-files state))
                               modified)]
                (if-not (seq modified)
                  (recur state (inc i))
                  (do (prn [:reloading-modified])
                      (recur (-> state
                                 (cljs/reload-modified-files! modified)
                                 (callback (mapv :name modified))
                                 (notify! modified))
                             (inc i)))

                  ))))))


    (let [in (-> System/in
                 (InputStreamReader.)
                 (BufferedReader.))]
      (prn [:repl-ready])
      (loop []
        (let [msg (.readLine in)]
          (when-not (nil? msg)
            (when (not= msg ":cljs/quit")
              (>!! repl-input msg)
              (recur)))))
      (prn [:repl-quit])
      )))
