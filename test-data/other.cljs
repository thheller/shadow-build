(ns other
  (:require [clojure.string :as str]
            ))

(defn sup []
  (.log js/console "sup"))

;; not exported but called
(sup)

(.log js/console "about to explode" (vals {:a "b"}))

(defn ex! []
  (throw (ex-info "who be callin?" {})))

(ex!)
