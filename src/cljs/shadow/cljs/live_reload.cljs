(ns shadow.cljs.live-reload
  (:require [cljs.reader :refer (read-string)]
            [goog.net.jsloader :as loader]))


(defonce scripts-to-load (atom []))

(defn handle-changes [{:keys [public-path before-load after-load] :as config} {:keys [js] :as changes}]
  (when (seq js)
    (when before-load
      (let [fn (js/goog.getObjectByName before-load)]
        (.debug js/console "Executing :before-load" before-load)
        (fn)
        ))

    (doseq [{:keys [name provides]} js]
      (.debug js/console "LOAD JS: " name (->> provides (map str) (into-array))))

    (swap! scripts-to-load into (->> js
                                     (map :js-name)
                                     (map #(str public-path "/src/" % "?r=" (rand)))
                                     ))

    (let [after-load-fn (fn []
                          (when after-load
                            (let [fn (js/goog.getObjectByName after-load)]
                              (.debug js/console "Executing :after-load " after-load)
                              (fn))))

          load-next (fn load-next []
                      (if-let [next (first @scripts-to-load)]
                        (do (swap! scripts-to-load rest)
                            (-> (loader/load next)
                                (.addBoth load-next)))
                        (after-load-fn)))]
      (load-next))
    ))

(defn setup [{:keys [socket-url] :as config}]
  (.debug js/console "LIVE RELOAD:" (pr-str config))
  (let [socket (js/WebSocket. socket-url)]
    (set! (.-onmessage socket) (fn [e]
                                 (handle-changes config (-> e .-data read-string))))
    (set! (.-onopen socket) (fn [e]
                              ;; patch away the already declared exception
                              (set! (.-provide js/goog) (aget js/goog "exportPath_"))
                              (.log js/console "websocket connected")))
    (set! (.-onclose socket) (fn [e]))
    (set! (.-onerror socket) (fn [e]))
    socket))

;; shadow.cljs.api/build-dev will append a
;; (setup my-config) here

