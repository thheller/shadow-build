(ns page)

(defn ^:export start-workers [log]
  (let [w1 (js/Worker. "/out/worker1.js")
        w2 (js/Worker. "/out/worker2.js")]

    (.addEventListener w1 "message" (fn [ev]
                                      (.log js/console "worker1:" ev)))
    (.addEventListener w2 "message" (fn [ev]
                                      (.log js/console "worker2:" ev)))))
