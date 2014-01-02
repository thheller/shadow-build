(ns worker2)

(js/postMessage (str "this is from worker2 " (pr-str {:dummy true})))
