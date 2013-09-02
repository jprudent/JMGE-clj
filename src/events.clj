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

(defmethod apply-event GameStarted
  [game {wall :wall}]
  (->> game
       (update-round (new-round wall))
       (draw-tile)))

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
  [game & _]
  {:pre [(every? #(= :wait-next-turn (get-player-state game %)) winds)]}

  (let [new-turn (init-turn (get-next-player game))]
    (->> game
         (update-turn new-turn)
         (draw-tile))))

(defn- punged
  "Update the game when a player punged"
  [game player]

  (let [last-discarded (get-last-discarded game)
        player-turn (get-player-turn game)
        new-fan (create-fan :pung last-discarded)
        new-turn (init-turn player)]
    (->> game
         (remove-from-player-tiles player 2 last-discarded)
         (remove-last-discarded)
         (add-player-fan player new-fan)
         (update-turn new-turn))))

(defn- chowed
  "Update the game when a player punged"
  [game player])

(defn- konged
  "Update the game when a player punged"
  [game player])

(defn- huled
  "Update the game when a player punged"
  [game player])

(defn- greater [[_ a-state & _ :as a] [_ b-state & _ :as b]]
  (let [order [:hule :kong :pung :chow :wait-next-turn]
        a-val (.indexOf order a-state)
        b-val (.indexOf order b-state)
        a-is-greater ( > 0 (- a-val b-val))]
    (if a-is-greater a b)))

(defn- apply-max-auction [game]
  (let [prepend-player-to-state #(into [%] (flatten [(get-player-state game %)]))
        prepended-player-to-states (map prepend-player-to-state (get-not-player-turnz game))
        find-max-auction (fn [] (reduce greater prepended-player-to-states))]

  (if (end-turn? game)
    (let [[player state & _] (find-max-auction)]
      (println player state)
      ((state {:wait-next-turn all-passed ;todo multimethod
               :chow chowed
               :pung punged
               :kong konged
               :hule huled}) game player))
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
