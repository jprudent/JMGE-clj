(ns majiang)

;; utility function
(defn exception [msg]
  (throw (Exception. msg)))

(defprotocol CommandHandler
  "A protocol for command handling"
  (perform [this game]))

(defmulti apply-event (fn [game event] (class event)))

(defn apply-events [state events]
  (reduce apply-event state events))

(defprotocol EventStore
  (retrieve-event-stream [this aggregate-id])
  (append-events [this aggregate-id previous-event-stream events])
  (clear-events [this aggregate-id]))

(defrecord EventStream [version transactions])

(import java.util.concurrent.ConcurrentHashMap)
(import java.util.ConcurrentModificationException)

(def in-memory-event-store
  (let [streams (ConcurrentHashMap.)
        empty-stream (->EventStream 0 [])]
    (reify EventStore
      (retrieve-event-stream [this aggregate-id]
        (if (.putIfAbsent streams aggregate-id empty-stream)
          (.get streams aggregate-id)
          empty-stream))

      (append-events [this aggregate-id previous-es events]
        (let [next-es (->EventStream (inc (:version previous-es))
                                     (conj (:transactions previous-es)
                                     events))
              replaced (.replace streams aggregate-id previous-es next-es)]
          (when-not replaced (throw (ConcurrentModificationException.)))))

      (clear-events [this aggregate-id]
                    (.put streams aggregate-id empty-stream)))))



;; Model
(def winds [:east :north :west :south])
(def all-tiles (flatten (conj
                         (take 4 (repeat [:b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9
                                          :c1 :c2 :c3 :c4 :c5 :c6 :c7 :c8 :c9
                                          :s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s9
                                          :we :wn :ww :ws
                                          :dr :dg :dw]))
                         [:fp :fo :fc :fb :ss :su :sa :sw])))

(defrecord Game [current-round nb-active-players])
(def empty-game (->Game nil 0))
(defrecord Round [current-hand remaining-prevalent-wind])
(defrecord Hand [current-turn wall player-hands])
(defrecord Turn [player])

;; Events
(defrecord PlayerJoined [aggregate-id wind])
(defrecord GameStarted [aggregate-id dice-thown-1 dice-thrown-2 wall])
(defrecord TurnStarted [aggregate-id])

(defmethod apply-event PlayerJoined [game event]
  (assoc game :nb-active-players (inc (:nb-active-players game))))

(defn- take-tile [{[t & ts] :source destination :destination :as tile-move}]
  {:pre [(not(nil? t))]}
  (assoc tile-move :source ts :destination (conj destination t)))

(defn- take-tiles [tile-move]
  (loop [nb-tiles (:nb-tiles tile-move)
         tile-move tile-move]
    (if (= 0 nb-tiles)
      tile-move
      (recur (dec nb-tiles) (take-tile tile-move)))))

(defn- initial-hands [wall]
    (reduce
     #(let [{player-hand :destination wall :source} (take-tiles {:source (:wall %1) :destination (list) :nb-tiles 13})
            current-player-hands (:player-hands %1)]
       (assoc %1, :player-hands (assoc current-player-hands %2 player-hand), :wall wall))
     {:wall wall :player-hands {}} winds))

(defn- new-hand [wall]
  (merge (->Hand (->Turn :east) nil nil) (initial-hands wall)))

(defmethod apply-event GameStarted [game event]
  (assoc game :current-round (->Round (new-hand (:wall event)) winds) ))

;; Commands
(defrecord NewPlayerEnter [aggregate-id])

(defn- throw-dice [] (+ 1 (rand-int 6)))
(defn- new-wall [] all-tiles)

(extend-protocol CommandHandler
  NewPlayerEnter
    (perform [this game]
      (cond
       (< (:nb-active-players game) 3) [(->PlayerJoined (:aggregate-id this) (:nb-active-players game))]
       (= (:nb-active-players game) 3) [(->PlayerJoined (:aggregate-id this) (:nb-active-players game))
                                        (->GameStarted (:aggregate-id this) (throw-dice) (throw-dice) (new-wall))
                                       ]
       :else (exception "Already 4 players"))))


(defn replay-all [aggregate-id]
  "Replay all events returning the resulting aggregate"
  (apply-events empty-game (flatten (:transactions (retrieve-event-stream in-memory-event-store aggregate-id)))))

(defn handle-command [command event-store]
  (let [event-stream (retrieve-event-stream event-store (:aggregate-id command))
        old-events (flatten (:transactions event-stream))
        current-state (apply-events empty-game old-events)
        new-events (perform command current-state)]
    (append-events event-store (:aggregate-id command) event-stream new-events)))



