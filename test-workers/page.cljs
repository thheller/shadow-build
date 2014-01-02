(ns page)

(defn ^:export start-workers [log]
  (let [w1 (js/Worker. "target/cljs/worker1.js")
        w2 (js/Worker. "target/cljs/worker2.js")]

    (set! (.-onmessage w1)
          (fn [ev]
            (.log js/console "worker1:" ev)))
    (set! (.-onmessage w2)
          (fn [ev]
            (.log js/console "worker2:" ev)))
    ))
