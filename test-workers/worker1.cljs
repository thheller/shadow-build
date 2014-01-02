(ns worker1)

(.log js/console "this is worker1")

(js/postMessage (str "this is from worker1 " (pr-str {:dummy true})))
