(ns org.liprudent.majiang.events
  (:require [org.liprudent.majiang.event-sourcing :as es]
            [org.liprudent.majiang.model :as m]
            [org.liprudent.majiang.utils :as u]))

(defrecord PlayerJoined [aggregate-id])
(defrecord GameStarted [aggregate-id dice-thown-1 dice-thrown-2 wall])
(defrecord TileDiscarded [aggregate-id player tile])
(defrecord Passed [aggregate-id player])
(defrecord Chowed [aggregate-id player owned-tiles])
(defrecord Punged [aggregate-id player])
(defrecord Konged [aggregate-id player])
(defrecord Huled [aggregate-id player hule-details])

;;; PlayerJoined

(defmethod es/apply-event PlayerJoined
  [game event]
  (m/inc-nb-active-players game))

;;; GameStarted

(defmethod es/apply-event GameStarted
  [game {wall :wall}]
  (->> game
    (m/update-round (m/new-round wall))
    (m/draw-tile)))

;;; TileDiscarded

(defmethod es/apply-event TileDiscarded
  [game {player :player discarded-tile :tile}]

  (let [new-player-discarded (conj (m/get-player-discarded game player) discarded-tile)
        new-player-tiles (u/minus (m/get-player-tiles game player) [discarded-tile])
        new-players-states (reduce #(assoc %1 %2 (if (= player %2) :wait-next-turn :auction )) {} m/winds)]
    (->> game
      (m/update-player-discarded player new-player-discarded)
      (m/update-player-tiles player new-player-tiles)
      (m/update-players-states new-players-states))))


(defn- all-passed
  "Update the game when all players passed for the discarded tile"
  [game & _]
  {:pre [(every? #(= :wait-next-turn (m/get-player-state game %)) m/winds)]}

  (let [new-turn (m/init-turn (m/get-next-player game))]
    (->> game
      (m/update-turn new-turn)
      (m/draw-tile))))

(defn- pungished
  [game player auction]
  (let [nb-tiles (if (= auction :pung ) 2 3)
        last-discarded (m/get-last-discarded game)
        player-turn (m/get-player-turn game)
        new-fan (m/create-fan auction last-discarded)
        new-turn (m/init-turn player)]
    (->> game
      (m/remove-from-player-tiles player nb-tiles last-discarded)
      (m/remove-last-discarded)
      (m/add-player-fan player new-fan)
      (m/update-turn new-turn))))


(defn- punged
  "Update the game when a player punged"
  [game player]
  (pungished game player :pung ))

(defn- konged
  "Update the game when a player konged"
  [game player]
  (m/draw-tile (pungished game player :kong )))

(defn- chowed
  "Update the game when a player chowed"
  [game player]

  (let [[t1 t2 :as tiles] (seq ((m/get-player-state game player) 1))
        last-discarded (m/get-last-discarded game)
        player-turn (m/get-player-turn game)
        new-fan (m/create-fan :chow last-discarded t1 t2)
        new-turn (m/init-turn player)]

    (->> game
      (m/remove-from-player-tiles player 1 t1)
      (m/remove-from-player-tiles player 1 t2)
      (m/remove-last-discarded)
      (m/add-player-fan player new-fan)
      (m/update-turn new-turn))))



(defn- huled
  "Update the game when a player huled"
  [game player])

(defn greater-auction [[_ a-state & _ :as a] [_ b-state & _ :as b]]
  (let [order [:hule :kong :pung :chow :wait-next-turn ]
        a-val (.indexOf order a-state)
        b-val (.indexOf order b-state)
        a-is-greater (> 0 (- a-val b-val))]
    (if a-is-greater a b)))

(defn- apply-max-auction [game]
  (let [prepend-player-to-state #(into [%] (flatten [(m/get-player-state game %)]))
        prepended-player-to-states (map prepend-player-to-state (m/get-not-player-turnz game))
        find-max-auction (fn [] (reduce greater-auction prepended-player-to-states))]

    (if (m/end-turn? game)
      (let [[player state & _] (find-max-auction)]
        ((state {:wait-next-turn all-passed ;todo multimethod
                 :chow chowed
                 :pung punged
                 :kong konged
                 :hule huled}) game player))
      game)))


;;; Passed

(defmethod es/apply-event Passed
  [game {player :player}]
  (let [update-player-turn-state #(assoc-in %
                                    [:current-round :current-hand :current-turn :player-states player]
                                    :wait-next-turn )]
    (apply-max-auction (update-player-turn-state game))))


(defn- update-state-auctioned [game player state]
  (assoc-in game
    [:current-round :current-hand :current-turn :player-states player]
    state))

;;; Chowed

(defmethod es/apply-event Chowed
  [game {player :player owned-tiles :owned-tiles}]
  (update-state-auctioned game player [:chow owned-tiles]))

;;; Punged

(defmethod es/apply-event Punged
  [game {player :player}]
  (update-state-auctioned game player :pung ))


;;; Konged

(defmethod es/apply-event Konged
  [game {player :player}]
  (update-state-auctioned game player :kong ))

;;; Huled

(defn last-hand?
  [game]
  (= (m/get-remaining-dealers game) [:south ]))

(defn last-round?
  [game]
  (= (m/get-remaining-prevalent-wind game) [:south ]))

(defn end-game? [game] (and (last-hand? game) (last-round? game)))

(defmethod es/apply-event Huled
  [game {player :player}]
  )
