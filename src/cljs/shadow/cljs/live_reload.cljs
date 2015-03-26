(ns shadow.cljs.live-reload
  (:require [cljs.reader :as reader]
            [goog.net.jsloader :as loader]))


(defonce scripts-to-load (atom []))

(def debug)

(defn debug
  ([a1]
    (when (aget js/window "console" "debug")
      (.debug js/console a1)))
  ([a1 a2]
    (when (aget js/window "console" "debug")
      (.debug js/console a1 a2)))
  ([a1 a2 a3]
    (when (aget js/window "console" "debug")
      (.debug js/console a1 a2 a3)))
  )

(def loaded? js/goog.isProvided_)

(defn handle-changes [{:keys [public-path before-load after-load] :as config} {:keys [js] :as changes}]
  (let [js-to-reload (->> js
                          ;; only reload things we actually require'd somewhere
                          (filter (fn [{:keys [provides]}]
                                    (some #(loaded? (str %)) provides)))
                          (into []))]

    (when (seq js-to-reload)
      (when before-load
        (let [fn (js/goog.getObjectByName before-load)]
          (debug "Executing :before-load" before-load)
          (fn)
          ))

      (doseq [{:keys [name provides]} js-to-reload]
        (debug "LOAD JS: " name (->> provides (map str) (into-array))))

      (swap! scripts-to-load into (->> js-to-reload
                                       (map :js-name)
                                       (map #(str public-path "/src/" % "?r=" (rand)))
                                       ))

      (let [after-load-fn (fn []
                            (when after-load
                              (let [fn (js/goog.getObjectByName after-load)]
                                (debug "Executing :after-load " after-load)
                                (fn))))

            load-next (fn load-next []
                        (if-let [next (first @scripts-to-load)]
                          (do (swap! scripts-to-load rest)
                              (-> (loader/load next)
                                  (.addBoth load-next)))
                          (after-load-fn)))]
        (load-next)))))

(defn setup [{:keys [socket-url] :as config}]
  (debug "LIVE RELOAD:" (pr-str config))
  ;; FIXME: fallback for IE?
  (when (aget js/window "WebSocket")
    (let [socket (js/WebSocket. socket-url)]
      (set! (.-onmessage socket) (fn [e]
                                   (handle-changes config (-> e .-data (reader/read-string)))))
      (set! (.-onopen socket) (fn [e]
                                ;; patch away the already declared exception
                                (set! (.-provide js/goog) (aget js/goog "exportPath_"))
                                (.log js/console "LIVE RELOAD: connected!")))
      (set! (.-onclose socket) (fn [e]
                                 ;; not a big fan of reconnecting automatically since a disconnect
                                 ;; may signal a change of config, safer to just reload the page
                                 (.warn js/console "LIVE RELOAD disconnected! (reload page to reconnect)")))
      (set! (.-onerror socket) (fn [e]))
      socket)))

;; shadow.cljs.api/build-dev will append a
;; (setup my-config) here

