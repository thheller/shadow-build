(ns shadow.cljs.live-reload
  (:require [cljs.reader :as reader]
            [goog.dom :as gdom]
            [goog.net.jsloader :as loader]
            ))


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
     (.debug js/console a1 a2 a3))))

(def loaded? js/goog.isProvided_)


(defn load-scripts
  [{:keys [public-path] :as config} filenames after-load-fn]
  (swap! scripts-to-load into filenames)

  (let [load-next (fn load-next []
                    (if-let [next (first @scripts-to-load)]
                      (do (swap! scripts-to-load (fn [remaining]
                                                   ;; rest will result in () if nothing is left
                                                   ;; we need to keep this a vector
                                                   (into [] (rest remaining))))
                          (debug "LOAD JS: " next)
                          (-> (loader/load (str public-path "/src/" next "?r=" (rand)))
                              (.addBoth (fn []
                                          (aset js/goog.included_ next true)
                                          (load-next)))))
                      (after-load-fn)))]
    (load-next)))


(defn handle-js-changes [{:keys [public-path before-load after-load] :as config} js]
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

      (let [after-load-fn (fn []
                            (when after-load
                              (let [fn (js/goog.getObjectByName after-load)]
                                (debug "Executing :after-load " after-load)
                                (fn))))]
        (load-scripts
          config
          (map :js-name js-to-reload)
          after-load-fn)))))

(defn handle-css-changes [config data]
  (doseq [[package-name package-info] data
          :let [{:keys [manifest path]} package-info]
          [css-name css-path] manifest]
    (when-let [node (js/document.querySelector (str "link[data-css-package=\"" package-name "\"][data-css-module=\"" css-name "\"]"))]
      (let [parent (gdom/getParentElement node)
            full-path (str path "/" css-path)
            new-link (doto (js/document.createElement "link")
                       (.setAttribute "rel" "stylesheet")
                       (.setAttribute "href" (str full-path "?r=" (rand)))
                       (.setAttribute "data-css-package" package-name)
                       (.setAttribute "data-css-module" css-name))]
        (debug (str "CSS: reload \"" full-path "\""))
        (gdom/insertSiblingAfter new-link node)
        (gdom/removeNode node)
        ))))

(defn repl-print [value]
  (js/console.log "repl-print" value)
  (pr-str value))

(defn repl-invoke
  [{:keys [repl-state socket] :as config}
   msg]
  (let [data (:js msg)
        result {:id (:id msg)
                :type :repl/result}
        result
        (try
          (js/console.log "eval" data)
          (let [ret (js/eval data)]
            (set! *3 *2)
            (set! *2 *1)
            (set! *1 ret)

            (try
              (assoc result
                :value (repl-print ret))
              (catch :default e
                (js/console.log "encoding of result failed" e ret)
                (assoc result :error "ENCODING FAILED"))))
          (catch :default e
            (set! *e e)
            (js/console.log "repl/invoke error" (pr-str msg) e)
            (assoc result :error (pr-str e))))]

    (.send socket (pr-str result))))

(defn repl-require [config {:keys [js-sources] :as msg}]
  (js/console.log "repl/require" (pr-str js-sources) (pr-str msg))
  (load-scripts
    config
    ;; don't load if already loaded
    (->> js-sources
         (remove #(aget js/goog.included_ %)))
    (fn []
      (js/console.log "repl-require finished"))))

(defn repl-init [config {:keys [repl-state]}]
  (js/console.log "repl config" (pr-str config))
  (js/console.log "repl init" (pr-str repl-state))
  (load-scripts
    config
    ;; don't load if already loaded
    (->> (:repl-js-sources repl-state)
         (remove #(aget js/goog.included_ %)))
    (fn [] (js/console.log "repl init complete"))))

(defn handle-message [config {:keys [type] :as msg}]
  (case type
    :js (handle-js-changes config (:data msg))
    :css (handle-css-changes config (:data msg))
    :repl/invoke (repl-invoke config msg)
    :repl/require (repl-require config msg)
    :repl/init (repl-init config msg)))

(defn setup [{:keys [socket-url] :as config}]
  (debug "LIVE RELOAD:" (pr-str config))
  ;; FIXME: fallback for IE?
  (when (aget js/window "WebSocket")
    (let [socket (js/WebSocket. socket-url)
          repl-state (atom {})
          config (assoc config
                   :socket socket
                   :repl-state repl-state)]

      (set! (.-onmessage socket)
            (fn [e]
              (handle-message config (-> e .-data (reader/read-string)))))

      (set! (.-onopen socket)
            (fn [e]
              ;; patch away the already declared exception
              (set! (.-provide js/goog) js/goog.constructNamespace_)
              (.log js/console "LIVE RELOAD: connected!")))
      (set! (.-onclose socket)
            (fn [e]
              ;; not a big fan of reconnecting automatically since a disconnect
              ;; may signal a change of config, safer to just reload the page
              (.warn js/console "LIVE RELOAD disconnected! (reload page to reconnect)")))

      (set! (.-onerror socket)
            (fn [e]))

      socket)))

