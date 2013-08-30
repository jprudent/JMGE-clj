(ns majiang (:gen-class :main true))

(load "event_sourcing")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility, general purpose function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn exception [msg]
  (throw (Exception. msg)))

(defn minus [v1 v2]
  "substract 2 vectors, v2 must be a subset of v1"
  (reduce (fn [acc v]
            (let [index (.indexOf acc v)]
              (into (subvec acc 0 index) (subvec acc (inc index))))) v1 v2))

(defn- move-tile
  "move a tile from source to destination"
  [{[t & ts] :source destination :destination :as tile-move}]
  {:pre [(not (nil? t))]}

  (assoc tile-move :source ts :destination (conj destination t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def winds [:east :north :west :south ])
(def all-tiles (into [:fp :fo :fc :fb :ss :su :sa :sw ]
                     (flatten (take 4 (repeat [:b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9
                                      :c1 :c2 :c3 :c4 :c5 :c6 :c7 :c8 :c9
                                      :s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s9
                                      :we :wn :ww :ws :dr :dg :dw ])))))

(defrecord Game [current-round nb-active-players])
(def empty-game (->Game nil 0))
(defrecord Round [current-hand remaining-prevalent-wind])
(defrecord Hand [current-turn wall player-hands discarded])
(defrecord Turn [player player-states])
(defn init-hand [player-turn]
  (->Hand (->Turn player-turn {}) nil nil {:east [] :north [] :west [] :south []}))

; various accessors
(defn get-hand [game] (:current-hand (:current-round game)))
(defn get-turn [game] (:current-turn (get-hand game)))
(defn get-player-tiles [game player] (player (:player-hands (get-hand game))))
(defn get-player-state [game player] (get-in (get-turn game) [:player-states player]))
(defn get-player-turn [game] (:player (get-turn game)))

; various tests
(defn tile-owned? [game player tile] (some #(= % tile) (get-player-tiles game player)))
(defn can-play? [game player] (not (= :wait-next-turn (get-player-state game player))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Events
;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord PlayerJoined [aggregate-id wind])
(defrecord GameStarted [aggregate-id dice-thown-1 dice-thrown-2 wall])
(defrecord TileDiscarded [aggregate-id player tile])

;;; PlayerJoined

(defmethod apply-event PlayerJoined [game event]
  (assoc game :nb-active-players (inc (:nb-active-players game))))

;;; GameStarted

(defn- move-tiles [{nb-tiles :nb-tiles [:as source] :source [:as destination] :destination :as tile-move}]
  {:pre [(>= (count source) nb-tiles)]}
  (let [taken (subvec source 0 nb-tiles)
        new-source (subvec source nb-tiles)
        new-destination (into taken destination)]
    (assoc tile-move :source new-source :destination new-destination)))

(defn- initial-hands-and-wall [wall]
    (reduce
     #(let [{player-hand :destination wall :source} (move-tiles {:source (:wall %1) :destination (list) :nb-tiles 13})
            current-player-hands (:player-hands %1)]
       (assoc %1, :player-hands (assoc current-player-hands %2 player-hand), :wall wall))
     {:wall wall :player-hands {}} winds))

(defn- draw-tile [hand to-player]
  (let [player-hands (:player-hands hand)
        player-hand (to-player player-hands)
        wall (:wall hand)
        tile-move {:source wall :destination player-hand}
        {new-wall :source new-hand :destination} (move-tile tile-move)
        hand-with-updated-wall (assoc hand :wall new-wall)]
    (assoc-in hand-with-updated-wall [:player-hands to-player] new-hand)))

(defn- new-hand [wall]
  (let [player-turn :east hand-after-wall-drawn (merge (init-hand player-turn) (initial-hands-and-wall wall))]
    (draw-tile hand-after-wall-drawn player-turn)))

(defmethod apply-event GameStarted [game event]
  (assoc game :current-round (->Round (new-hand (:wall event)) winds)))

;;; TileDiscarded

(defmethod apply-event TileDiscarded [game event]
  (let [player (:player event)
        player-tiles (get-player-tiles game player)
        player-discarded (player (:discarded (get-hand game)))
        tile-move {:source player-tiles :destination player-discarded}
        {new-player-discarded :destination new-player-tiles :source} (move-tile tile-move)]
    (assoc-in
      (assoc-in
        (assoc-in game [:current-round :current-hand :discarded player] new-player-discarded)
        [:current-round :current-hand :player-hands player] new-player-tiles)
      [:current-round :current-hand :current-turn :player-states player] :wait-next-turn)))

;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;

(defrecord NewPlayerEnter [aggregate-id])
(defrecord DiscardTile [aggregate-id player tile])

(defn- throw-dice [] (+ 1 (rand-int 6)))
(defn- new-wall [] all-tiles) ;todo shuffle

(extend-protocol CommandHandler

  NewPlayerEnter
  (perform [this game]
    (cond
      (< (:nb-active-players game) 3) [(->PlayerJoined (:aggregate-id this) (:nb-active-players game))]
      (= (:nb-active-players game) 3) [(->PlayerJoined (:aggregate-id this) (:nb-active-players game))
                                       (->GameStarted (:aggregate-id this) (throw-dice) (throw-dice) (new-wall))]
      :else (exception "Already 4 players")))

  DiscardTile
  (perform [this game]
    (if
      (and
        (= (:player this) (get-player-turn game))
        (tile-owned? game (:player this) (:tile this))
        (can-play? game (:player this))
       )
      [(->TileDiscarded (:aggregate-id this) (:player this) (:tile this))]
      (exception "Not player turn or the tile doesn't belong to player"))))



(defn handle-command
  "Interface between commands and event-store"
  [command event-store]
  (let [event-stream (retrieve-event-stream event-store (:aggregate-id command))
        old-events (flatten (:transactions event-stream))
        current-state (apply-events empty-game old-events)
        new-events (perform command current-state)]
    (append-events event-store (:aggregate-id command) event-stream new-events)))






