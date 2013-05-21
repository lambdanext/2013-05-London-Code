(ns tron.bots
  (:require [tron.core :as tron]
    tron.bots.ahoy
    tron.bots.gav
    tron.bots.neil
    tron.bots.xianralph
    tron.bots.braindead
    tron.bots.kubibot
    tron.bots.phil
    tron.bots.paddy
    tron.bots.squiggly))

(defn buzz 
  "To infinity and beyond!"
  [look {[x y] :pos}]
  {:pos [(inc x) y]})

(defn right [[x y]]
  [(inc x) y])

(defn down [[x y]]
  [x (inc y)])

(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))

;; launch your favorite (random) bot
#_(tron/spawn-biker "tcp://172.16.25.93:5555"
    (rand-nth [tron.bots.ahoy/turn-based-strat
               tron.bots.gav/better-buzz
               tron.bots.neil/circle
               tron.bots.xianralph/switcheroo
               tron.bots.braindead/simple-buzz
               tron.bots.kubibot/kubibot
               tron.bots.phil/home-b
               tron.bots.paddy/max-moves-strategy
               tron.bots.squiggly/move]))
