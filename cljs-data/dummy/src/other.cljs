(ns other
  (:require [clojure.string :as str]
            ))

(defn sup []
  (.log js/console "sup" :look))

;; not exported but called
(sup)
