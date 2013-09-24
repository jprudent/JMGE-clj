(defrecord NewPlayerEnter [aggregate-id])
(defrecord DiscardTile [aggregate-id player tile])
(defrecord Pass [aggregate-id player])
(defrecord Chow [aggregate-id player owned-tiles])
(defrecord Pung [aggregate-id player])
(defrecord Kong [aggregate-id player])
(defrecord Hule [aggregate-id player])


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

(defn hule-details
  "TODO gros mock de la more, utiliser JMHC.clj"
  [game player]
  [;retourne un tableau, avec le plus gros score possible en premier
   {:total 120}])

(extend-protocol CommandHandler

  NewPlayerEnter
  (perform [{aggregate-id :aggregate-id} game]
    (cond
      (< (:nb-active-players game) 3) [(->PlayerJoined aggregate-id)]
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
      (exception "Player " player " can't pass " game "fooo")))

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
    (pungish-perform event game #(->Konged aggregate-id player) 3))

  Hule
  (perform [{aggregate-id :aggregate-id player :player :as event} game]
    (if (can-auction? game player)
      (let [hule-details (hule-details game player)]
        (if (valid-hule? hule-details)
          [(->Huled aggregate-id player hule-details)]
          (exception "Hule invalid")))
      (exception "Player can't hule"))))

(defn handle-command
  "Interface between commands and event-store"
  [command event-store]
  (let [event-stream (retrieve-event-stream event-store (:aggregate-id command))
        old-events (flatten (:transactions event-stream))
        current-state (apply-events empty-game old-events)
        new-events (perform command current-state)]
    (append-events event-store (:aggregate-id command) event-stream new-events)))
