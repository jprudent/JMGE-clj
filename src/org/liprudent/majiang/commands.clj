(ns org.liprudent.majiang.commands
  (:require [org.liprudent.majiang.utils :as u]
            [org.liprudent.majiang.model :as m]
            [org.liprudent.majiang.events :as e]
            [org.liprudent.majiang.event-sourcing :as es]))

(defrecord NewPlayerEnter [aggregate-id])
(defrecord DiscardTile [aggregate-id player tile])
(defrecord Pass [aggregate-id player])
(defrecord Chow [aggregate-id player owned-tiles])
(defrecord Pung [aggregate-id player])
(defrecord Kong [aggregate-id player])
(defrecord Hule [aggregate-id player])


(defn- throw-dice [] (+ 1 (rand-int 6)))
(defn- new-wall [] m/all-tiles) ;todo shuffle

(defn- count-tiles-of-type [game player tile]
  (let [last-discarded (m/get-last-discarded game)
        get-player-tiles #(m/get-player-tiles game player)
        inc-if #(if %1 (inc %2) %2)]
    (reduce #(inc-if (= last-discarded %2) %1) 0 (get-player-tiles))))


(defn- pungish-perform
  "a perform implementation for pung and kong commands"
  [{aggregate-id :aggregate-id player :player} game event min-count-in-hand]
  (let [last-discarded #(m/get-last-discarded game)
        count-in-hand #(count-tiles-of-type game player (last-discarded))]
    (if (and (m/can-auction? game player) (>= (count-in-hand) min-count-in-hand))
      [(event)]
      (u/exception "Player can't pung/kong"))))

(defn hule-details
  "TODO gros mock de la more, utiliser JMHC.clj"
  [game player]
  [;retourne un tableau, avec le plus gros score possible en premier
   {:total 120}])

(defprotocol CommandHandler
  ;"A protocol for command handling. Each command have to extend this protocol"
  (perform [this game]))

(extend-protocol CommandHandler

  NewPlayerEnter
  (perform [{aggregate-id :aggregate-id} game]
    (cond
      (< (:nb-active-players game) 3) [(e/->PlayerJoined aggregate-id)]
      (= (:nb-active-players game) 3) [(e/->PlayerJoined aggregate-id)
                                       (e/->GameStarted aggregate-id (throw-dice) (throw-dice) (new-wall))]
      :else (u/exception "Already 4 players")))

  DiscardTile
  (perform [{aggregate-id :aggregate-id player :player tile :tile} game]
    (if
      (and
        (= player (m/get-player-turn game))
        (m/tile-owned? game player tile)
        (m/can-discard? game player)
        )
      [(e/->TileDiscarded aggregate-id player tile)]
      (u/exception "Not player turn or the tile doesn't belong to player")))

  Pass
  (perform [{aggregate-id :aggregate-id player :player} game]
    (if (m/can-auction? game player)
      [(e/->Passed aggregate-id player)]
      (u/exception "Player " player " can't pass " game "fooo")))

  Chow
  (perform [{aggregate-id :aggregate-id player :player owned-tiles :owned-tiles} game]
    (if (and
          (m/can-auction? game player)
          (every? #(m/tile-owned? game player %1) owned-tiles)
          (= player (m/get-next-player game))
          (m/valid-chow? game owned-tiles))
      [(e/->Chowed aggregate-id player owned-tiles)]
      (u/exception "Player can't chow")))

  Pung
  (perform [{aggregate-id :aggregate-id player :player :as event} game]
    (pungish-perform event game #(e/->Punged aggregate-id player) 2))

  Kong
  (perform [{aggregate-id :aggregate-id player :player :as event} game]
    (pungish-perform event game #(e/->Konged aggregate-id player) 3))

  Hule
  (perform [{aggregate-id :aggregate-id player :player :as event} game]
    (if (m/can-auction? game player)
      (let [hule-details (hule-details game player)]
        (if (m/valid-hule? hule-details)
          [(e/->Huled aggregate-id player hule-details)]
          (u/exception "Hule invalid")))
      (u/exception "Player can't hule"))))

(defn handle-command
  "Interface between commands and event-store"
  [command event-store]
  (let [event-stream (es/retrieve-event-stream event-store (:aggregate-id command))
        old-events (flatten (:transactions event-stream))
        current-state (es/apply-events m/empty-game old-events)
        new-events (perform command current-state)]
    (es/append-events event-store (:aggregate-id command) event-stream new-events)))
