(ns tron.core
  (:require 
    [quil.core :as q]
    [clojure.edn :as edn]))

(def size "size of the square arena" 60)
(def scale 10)
(def sleep-length "time in ms between turns" 200)

(def arena
  (mapv vec (partition size
              (repeatedly (* size size) #(ref nil)))))

(defn blank-arena []
  (dosync
    (doseq [row arena r row]
      (ref-set r nil))))

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

(defn server
  "Starts the UI and dispatches to (local) workers" 
  []
  (q/defsketch tron
  :title "TRON"
  :setup setup
  :draw draw
  :size [(* scale size) (* scale size)])
  (my-with-open [context ^{:close-with .term} (org.jeromq.ZMQ/context 1)
                 router (doto (.socket context org.jeromq.ZMQ/ROUTER)
                          (.bind "tcp://*:5555"))
                 dealer (doto (.socket context org.jeromq.ZMQ/DEALER)
                          (.bind "tcp://*:5556"))]
    (.run (org.jeromq.ZMQQueue. context router dealer))))

(defn worker []
  (my-with-open [context ^{:close-with .term} (org.jeromq.ZMQ/context 1)
               socket (doto (.socket context org.jeromq.ZMQ/REP)
                        (.connect "tcp://localhost:5556"))]
    (while (not (.isInterrupted (Thread/currentThread)))
      (reply socket process-req))))

(defn request [socket msg]
  (send-msg socket msg)
  (recv-msg socket))

(defn client [addr name strategy]
  (my-with-open [context ^{:close-with .term} (org.jeromq.ZMQ/context 1)
                 socket (doto (.socket context org.jeromq.ZMQ/REQ)
                          (.connect addr))]
    (let [{:keys [hue id pos]} (request socket {:method :register
                                                :name name})]
      (loop [state {:pos pos}]
        (let [arena (request socket {:method :look})
              look (fn [pos] (get-in arena pos))
              state' (strategy look state)
              pos' (:pos state')
              {:keys [status]} (request socket {:method :move
                                                :id id
                                                :pos pos'})]
          (if (= :ok status)
            (do
              (Thread/sleep sleep-length)
              (recur state'))
            (println name "died")))))))

(defn spawn-biker 
  ([strategy]
    (spawn-biker "tcp://localhost:5555" strategy))
  ([addr strategy]
    (future (client addr (name (gensym "Bot")) strategy))))

;;;; Launch them all!!

#_(def srv (future (server)))

#_(def workers (vec (take 2 (repeatedly #(future (worker))))))

