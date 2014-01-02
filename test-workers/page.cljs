(ns page
  (:require-macros [shadow.macros :refer (log ns-ready)])
  (:require [shadow.dom :as dom]
            [shadow.object :as so]))

(defn ^:export start-workers [log]
  (let [w1 (js/Worker. "target/cljs/worker1.js")
        w2 (js/Worker. "target/cljs/worker2.js")]

    (set! (.-onmessage w1)
          (fn [ev]
            (so/log "worker1:" ev)
            (dom/append log [:div "worker1:" (.-data ev)])))
    (set! (.-onmessage w2)
          (fn [ev]
            (so/log "worker2:" ev)
            (dom/append log [:div "worker2:" (.-data ev)])))
    ))

(ns-ready)
