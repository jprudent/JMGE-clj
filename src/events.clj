(ns majiang)

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
  (inc-nb-active-players game))

;;; GameStarted

(defn- give-13-tiles [{:keys [player-hands wall] :as hand} to-player]
  (let [[new-wall new-player-hand] (move-tiles 13 wall (list))
        new-player-hands (assoc player-hands to-player new-player-hand)]
       (assoc hand :player-hands new-player-hands, :wall new-wall)))

(defn- initial-hands-and-wall [wall]
  (reduce give-13-tiles {:wall wall :player-hands {}} winds))

(defn- draw-tile

  "2 parameters :
  - hand is :current-hand
  - to-player is player that draws tile

  1 parameter :
  - game: the game with a tile drawed by player-turn"

  ([hand to-player]
  (let [player-hands (:player-hands hand)
        player-hand (to-player player-hands)
        wall (:wall hand)
        tile-move {:source wall :destination player-hand}
        {new-wall :source new-hand :destination} (move-tile tile-move)
        hand-with-updated-wall (assoc hand :wall new-wall)]
    (assoc-in hand-with-updated-wall [:player-hands to-player] new-hand)))

  ([game]
   (update-in game [:current-round :current-hand] draw-tile (get-player-turn game))))

(defn- new-hand [wall]
  (let [player-turn :east
        hand-after-wall-drawn (merge (init-hand player-turn) (initial-hands-and-wall wall))]
    (draw-tile hand-after-wall-drawn player-turn)))

(defmethod apply-event GameStarted
  [game event]

  (assoc game :current-round (->Round (new-hand (:wall event)) winds)))

;;; TileDiscarded

(defmethod apply-event TileDiscarded
  [game {player :player discarded-tile :tile}]

  (let [new-player-discarded (conj (get-player-discarded game player) discarded-tile)
        new-player-tiles (minus (get-player-tiles game player) [discarded-tile])
        new-players-states (reduce #(assoc %1 %2 (if (= player %2) :wait-next-turn :auction)) {} winds)]
    (->> game
         (update-player-discarded player new-player-discarded)
         (update-player-tiles player new-player-tiles)
         (update-players-states new-players-states))))


(defn- all-passed

  "Update the game when all players passed for the discarded tile"

  [game]
  {:pre [(every? #(= :wait-next-turn (get-player-state game %)) winds)]}

  (let [new-player-turn (get-next-player game)
        new-turn (init-turn new-player-turn)
        set-new-turn #(update-turn % new-turn)
        player-turn-draw-tile #(assoc-in % [:current-round :current-hand :player-hands] (draw-tile % ))]

    (draw-tile (set-new-turn game))))

(defn- punged

  "Update the game when a player punged"

  [game player]

  (let [set-new-turn #(update-turn % (init-turn player))

        player-turn-draw-tile #(assoc-in % [:current-round :current-hand :player-hands] (draw-tile % ))]
  ))

(defn- apply-max-auction [game]
  (let [greater? (fn [[_ a-state & _ :as a] b]
                    (cond (= :hule a-state) a
                          (or (= :pung a-state) (= :kong a-state)) a
                          (= :chow a-state) a
                          :else b))
        max-auction (fn [] (reduce greater?
                                  (map #(into [%] (flatten [(get-player-state game %)]))  (get-not-player-turnz game))))

        chowed (fn [[player _ owned-tile]])
        konged (fn [player])
        huled (fn [player])]

  (if (end-turn? game)

    (let [[_ state & _ :as max-auctioned] (max-auction)]
      ((state {:wait-next-turn all-passed
               :chow chowed
               :pung punged
               :kong konged
               :hule huled}) game))
    game)))


;;; Passed

(defmethod apply-event Passed
  [game {player :player}]
  (let [update-player-turn-state #(assoc-in %
                                            [:current-round :current-hand :current-turn :player-states player]
                                            :wait-next-turn)]
  (apply-max-auction (update-player-turn-state game))))


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
