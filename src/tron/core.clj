(ns tron.core
  (:require 
    [quil.core :as q]
    [clojure.edn :as edn]))

(def size "size of the square arena" 30)
(def scale 20)
(def sleep-length "time in ms between turns" 200)

(def arena
  (vec
    (map vec (partition size
               (repeatedly (* size size) #(ref nil))))))

(defn setup []
  (q/color-mode :hsb)
  (q/smooth)
  (q/frame-rate 10))

(defn draw []
  (q/background 0)
  (dosync 
    (doseq [x (range 0 size)
            y (range 0 size)]
      (when-let [hue @(get-in arena [x y])]
        (q/fill (q/color hue 255 255))
        (q/rect (* scale x) (* scale y) scale scale)))))

(q/defsketch tron
  :title "TRON"
  :setup setup
  :draw draw
  :size [(* scale size) (* scale size)])

(defn valid-pos? [[i j]]
  (and (< -1 i size) (< -1 j size)))

(def legal-moves #{[0 1] [1 0] [0 -1] [-1 0]})

(defn valid-move? [from to]
  (contains? legal-moves (map - to from)))

(defmacro my-with-open [bindings & body]
  (if-let [[name expr & more-bindings] (seq bindings)]
    (let [close (:close-with (meta expr) '.close)]
      `(let [~name ~expr]
         (try
           (my-with-open ~more-bindings ~@body)
           (finally
             (-> ~name ~close)))))
    `(do ~@body)))

(defmulti process-req :method)

(def clients
  (atom {}))

(def ids (atom 0))

(defmethod process-req :register [{:keys [name]}]
  (let [hue (rand-int 255)
        id (swap! ids inc) 
        pos [(rand-int size) (rand-int size)]]
    (swap! clients assoc id 
      {:hue hue :pos (ref pos) :id id :name name})
    (dosync (ref-set (get-in arena pos) hue))
    {:hue hue :pos pos :id id}))

(defmethod process-req :move [{:keys [id pos]}]
  (let [{hue :hue rpos :pos} (@clients id)]
    (if (valid-pos? pos)
      (let [cell (get-in arena  pos)
            moved (dosync 
                    (when (and (valid-move? @rpos pos) (nil? @cell))
                      (ref-set cell hue)
                      (ref-set rpos pos)
                      true))]
        {:status (if moved :ok :ko)})
      {:status :ko})))

(defmethod process-req :look [_]
  (mapv #(mapv deref %) arena))

(defn send-msg [^org.jeromq.ZMQ$Socket socket msg]
  (.send socket (.getBytes (pr-str msg) "UTF-8")))

(defn recv-msg [^org.jeromq.ZMQ$Socket socket]
  (edn/read-string (String. (.recv socket 0) "UTF-8")))

(defn reply [socket f]
  (->> (recv-msg socket) f (send-msg socket)))

(defn server []
  (my-with-open [context ^{:close-with .term} (org.jeromq.ZMQ/context 1)
                 socket (doto (.socket context org.jeromq.ZMQ/REP)
                          (.bind "tcp://*:5555"))]
    (while (not (.isInterrupted (Thread/currentThread)))
      (reply socket process-req))))

(defn request [socket msg]
  (send-msg socket msg)
  (recv-msg socket))

(defn client [name strategy]
  (my-with-open [context ^{:close-with .term} (org.jeromq.ZMQ/context 1)
                 socket (doto (.socket context org.jeromq.ZMQ/REQ)
                          (.connect "tcp://localhost:5555"))]
    (let [{:keys [hue id pos]} (request socket {:method :register
                                                :name name})]
      (loop [state {:pos pos}]
        (let [arena (request socket {:method :look})
              state' (strategy arena state)
              pos' (:pos state')
              {:keys [status]} (request socket {:method :move
                                                :id id
                                                :pos pos'})]
          (if (= :ok status)
            (do
              (Thread/sleep sleep-length)
              (recur state'))
            (println name "died")))))))

(defn buzz 
  "To the infinity and beyond!"
  [look {[x y] :pos}]
  {:pos [(inc x) y]})

;;;; Launch them all!!

#_(def srv (future (server)))

#_(def bots (doall (for [[n s] {"Bot1" buzz "Bot2" buzz}]
                       (future (client n s)))))


