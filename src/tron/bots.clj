(ns tron.bots
  (:require [tron.core :as tron]))

(defn buzz 
  "To the infinity and beyond!"
  [look {[x y] :pos}]
  {:pos [(inc x) y]})

;; launch two buzzes
#_(doseq [s [buzz buzz]]
  (tron/spawn-biker "tcp://172.16.25.93:5555" s))