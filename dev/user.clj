(ns user 
  (:require [vlaaad.reveal :as reveal]))

(defn run-reveal []
  (add-tap (reveal/ui)))

(comment
  (run-reveal)
  ,)

