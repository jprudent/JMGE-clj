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
(defrecord Hand [current-turn wall player-hands discarded fans])
(defrecord Turn [player player-states])
(defn init-hand [player-turn]
  (->Hand (->Turn player-turn {}) nil nil {:east [] :north [] :west [] :south []} {:east [] :north [] :west [] :south []}))

; various accessors
(defn get-hand [game] (:current-hand (:current-round game)))
(defn get-turn [game] (:current-turn (get-hand game)))
(defn get-player-tiles [game player] (player (:player-hands (get-hand game))))
(defn get-player-state [game player] (get-in (get-turn game) [:player-states player]))
(defn get-player-turn [game] (:player (get-turn game)))
(defn get-last-discarded [game] (last (get-in (get-hand game) [:discarded (get-player-turn game)])))
(defn get-next-player [game] (winds (mod (inc (.indexOf winds (get-player-turn game))) 4)))
(defn count-tiles-of-type [game player tile]
  (let [last-discarded (get-last-discarded game)
          get-player-tiles #(get-player-tiles game player)
          inc-if #(if %1 (inc %2) %2)]
       (reduce #(inc-if (= last-discarded %2) %1) 0 (get-player-tiles))))

; various tests
(defn tile-owned? [game player tile] (some #(= % tile) (get-player-tiles game player)))
(defn can-discard? [game player] (not (= :wait-next-turn (get-player-state game player))))
(defn can-auction? [game player] (= :auction (get-player-state game player)))
(defn has-fan? [game player fan] (some #(= fan %1) (get-in (get-hand game) [:fans player])))
(defn has-played-turn? [game player]
  (let [player-state (get-in game [:current-round :current-hand :current-turn :player-states player])]
    (or
     (not (nil? (some #(= % player-state) [:wait-next-turn :pung :kong :hule])))
     (and (vector? player-state) ( = :chow (first player-state))))))

; various tiles related functions
(defn- tile-to-char-seq [tile] (vec (str tile)))
(defn- char-to-int [c] (- (int c) (int \0)))

(defn family [tile] ((tile-to-char-seq tile) 1))
(defn order [tile]  (char-to-int ((tile-to-char-seq tile) 2)))
(defn to-tile [family order] (keyword (str family order)))

(defn valid-chow? [game owned-tiles]
  {:pre [(set? owned-tiles)]}
  (let [last-discarded (get-last-discarded game)
        expected-family (family last-discarded)
        order (order last-discarded)
        proposed-chow (conj owned-tiles last-discarded)
        to-chow (fn [orders] (conj
                              (reduce #(conj %1 (to-tile expected-family (+ order %2))) #{} orders)
                              last-discarded)) ]
    (not (nil? (some #(= proposed-chow %1) (map to-chow [[-2 -1] [-1 1] [1 2]]))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Events
;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord PlayerJoined [aggregate-id])
(defrecord GameStarted [aggregate-id dice-thown-1 dice-thrown-2 wall])
(defrecord TileDiscarded [aggregate-id player tile])
(defrecord Passed [aggregate-id player])
(defrecord Chowed [aggregate-id player owned-tiles])
(defrecord Punged [aggregate-id player])
(defrecord Konged [aggregate-id player])

;;; PlayerJoined

(defmethod apply-event PlayerJoined
  [game event]
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

(defmethod apply-event GameStarted
  [game event]

  (assoc game :current-round (->Round (new-hand (:wall event)) winds)))

;;; TileDiscarded

(defmethod apply-event TileDiscarded
  [game {player :player discarded-tile :tile}]

  (let [player-discarded (player (:discarded (get-hand game)))
        new-player-discarded (conj player-discarded discarded-tile)
        new-player-tiles (minus (get-player-tiles game player) [discarded-tile])
        new-player-states (reduce #(assoc %1 %2 (if (= player %2) :wait-next-turn :auction)) {} winds)]
    (assoc-in
      (assoc-in
        (assoc-in game [:current-round :current-hand :discarded player] new-player-discarded)
        [:current-round :current-hand :player-hands player] new-player-tiles)
      [:current-round :current-hand :current-turn :player-states] new-player-states)))



(defn- apply-max-auction [game]
  (let [not-player-turnz (minus winds (get-player-turn game))
       end-turn? (fn [] (every? #(has-played-turn? game %) not-player-turnz))
       greater? (fn [a b]
                  (cond (and (vector? a) (vector? b))
                        (let [a-claim (a 1)]
                             (cond (= :hule a-claim) a
                                   (or (= :pung a-claim) (= :kong a-claim)) a
                                   (= :chow a-claim) a
                                   :else b))
                        (vector? a) a
                        (vector? b) b))
       max-auction (fn [] (reduce greater? (map #(into [%] (get-player-state game %)) not-player-turnz)))
       apply-all-passed #()
       apply-chowed (fn [[player _ _ owned-tile]])
       apply-punged (fn [player])
       apply-konged (fn [player])
       apply-huled (fn [player])]

  (if (end-turn?)
    (let [max-auctioned (max-auction)]
      (cond (= :wait-next-turn max-auctioned) (apply-all-passed)
            (and (vector? max-auctioned) (= :chow (max-auctioned 1))) (apply-chowed max-auctioned)
            (and (vector? max-auctioned) (= :pung (max-auctioned 1))) (apply-punged max-auctioned)
            (and (vector? max-auctioned) (= :kong (max-auctioned 1))) (apply-konged max-auctioned)
            (and (vector? max-auctioned) (= :hule (max-auctioned 1))) (apply-huled max-auctioned))))))


;;; Passed

(defmethod apply-event Passed
  [game {player :player}]

  (assoc-in game [:current-round :current-hand :current-turn :player-states player] :wait-next-turn))

(defn- update-state-auctioned [game player state]
  (assoc-in game
            [:current-round :current-hand :current-turn :player-states player]
            state))

;;; Chowed
(defmethod apply-event Chowed
  [game {player :player owned-tiles :owned-tiles}]
  (update-state-auctioned game player [:chow owned-tiles]))

;;; Punged

(defmethod apply-event Punged
  [game {player :player}]
  (update-state-auctioned game player :pung))


;;; Konged

(defmethod apply-event Konged
  [game {player :player}]
  (update-state-auctioned game player :kong))

;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;

(defrecord NewPlayerEnter [aggregate-id])
(defrecord DiscardTile [aggregate-id player tile])
(defrecord Pass [aggregate-id player])
(defrecord Chow [aggregate-id player owned-tiles])
(defrecord Pung [aggregate-id player])
(defrecord Kong [aggregate-id player])


(defn- throw-dice [] (+ 1 (rand-int 6)))
(defn- new-wall [] all-tiles) ;todo shuffle

(defn- count-tiles-of-type [game player tile]
  (let [last-discarded (get-last-discarded game)
          get-player-tiles #(get-player-tiles game player)
          inc-if #(if %1 (inc %2) %2)]
       (reduce #(inc-if (= last-discarded %2) %1) 0 (get-player-tiles))))


(defn- pungish-perform
  "a perform implementation for pung and kong commands"
  [{aggregate-id :aggregate-id player :player} game event min-count-in-hand]
  (let [last-discarded #(get-last-discarded game)
        count-in-hand #(count-tiles-of-type game player (last-discarded))]
      (if (and (can-auction? game player) (>= (count-in-hand) min-count-in-hand))
          [(event)]
          (exception "Player can't pung/kong"))))

(extend-protocol CommandHandler

  NewPlayerEnter
  (perform [{aggregate-id :aggregate-id} game]
    (cond
      (< (:nb-active-players game) 3) [(->PlayerJoined aggregate-id )]
      (= (:nb-active-players game) 3) [(->PlayerJoined aggregate-id)
                                       (->GameStarted aggregate-id (throw-dice) (throw-dice) (new-wall))]
      :else (exception "Already 4 players")))

  DiscardTile
  (perform [{aggregate-id :aggregate-id player :player tile :tile} game]
    (if
      (and
        (= player (get-player-turn game))
        (tile-owned? game player tile)
        (can-discard? game player)
       )
      [(->TileDiscarded aggregate-id player tile)]
      (exception "Not player turn or the tile doesn't belong to player")))

  Pass
  (perform [{aggregate-id :aggregate-id player :player} game]
    (if (can-auction? game player)
      [(->Passed aggregate-id player)]
      (exception "Player can't make auction for discarded tile")))

  Chow
  (perform [{aggregate-id :aggregate-id player :player owned-tiles :owned-tiles} game]
    (if (and
         (can-auction? game player)
         (every? #(tile-owned? game player %1) owned-tiles)
         (= player (get-next-player game))
         (valid-chow? game owned-tiles))
      [(->Chowed aggregate-id player owned-tiles)]
      (exception "Player can't chow")))

  Pung
  (perform [{aggregate-id :aggregate-id player :player :as event} game]
           (pungish-perform event game #(->Punged aggregate-id player) 2))

  Kong
  (perform [{aggregate-id :aggregate-id player :player :as event} game]
           (pungish-perform event game #(->Konged aggregate-id player) 3)))

(defn handle-command
  "Interface between commands and event-store"
  [command event-store]
  (let [event-stream (retrieve-event-stream event-store (:aggregate-id command))
        old-events (flatten (:transactions event-stream))
        current-state (apply-events empty-game old-events)
        new-events (perform command current-state)]
    (append-events event-store (:aggregate-id command) event-stream new-events)))









