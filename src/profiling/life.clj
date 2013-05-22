(ns profiling.life
  (:require [quil.core :as q]))

(def world (atom #{}))

; computing neighbours
(defn neighbours [{:keys [x y]}]
  (for [nx (range (dec x) (+ x 2))
        ny (range (dec y) (+ y 2))
        :when (or (not= nx x)
                   (not= ny y))]
    {:x nx :y ny}))

; Any live cell with fewer than two live neighbours dies, as if caused by under-population.
; Any live cell with two or three live neighbours lives on to the next generation.
; Any live cell with more than three live neighbours dies, as if by overcrowding.
; Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

(defn stepper [neighbours survive? spawn?]
  (fn [cells]
    {:pre [(set? cells)]
     :post [(set? %)]}
    (let [fs (frequencies 
               (mapcat neighbours cells))]
      (set 
        (for [[cell n] fs
              :when (if (cells cell)
                      (survive? n)
                      (spawn? n))]
          cell)))))

(def step (stepper neighbours #{2 3} #{3}))

(def scale 4)
(def start-range 150)

(defn setup []
  (q/no-stroke)
  (q/no-smooth)
  (q/frame-rate 60)
  (q/background 0 0 255))

(defn draw []
  (q/background 0 0 255)
  (q/fill 255 255 0)
  (doseq [{:keys [x y]} @world]
    (q/rect (* scale x) (* scale y) scale scale)))       ;;Draw a circle at x y with the correct diameter

(q/defsketch life
  :title "life is good"
  :setup setup
  :draw draw
  :size [600 600])

(reset! world
  (set (repeatedly (/ (* start-range start-range) 4)
                   (fn []
                     {:x (rand-int start-range)
                      :y (rand-int start-range)}))))

(def stop (atom false))
(def f (future (while (not @stop) (swap! world step))))
