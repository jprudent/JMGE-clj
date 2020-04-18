(ns org.liprudent.majiang.model
  (:require [org.liprudent.majiang.utils :as u]))

(def winds [:east :north :west :south])
(def honors #{:we :wn :ww :ws :dr :dg :dw})
(def flowers+seasons #{:fp :fo :fc :fb :ss :su :sa :sw})
(def all-tiles (->> (concat [:b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4 :c5 :c6 :c7 :c8 :c9 :s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s9]
                            honors)
                    (cycle)
                    (take 136)
                    (concat flowers+seasons)
                    (vec)))
(def green-tiles #{:b2 :b3 :b4 :b6 :b8 :dg})
(def tile? (set all-tiles))

(defrecord Turn [player player-states])

(defn init-turn [player-turn] (->Turn player-turn {}))


(defrecord Hand [current-turn wall player-hands discarded fans remaining-dealers])

(defn- init-hand [player-turn]
  (let [empty-map {:east [] :north [] :west [] :south []}]
    (->Hand (init-turn player-turn) nil nil empty-map empty-map empty-map)))

(defn- give-13-tiles [{:keys [player-hands wall] :as hand} to-player]
  (let [[new-wall new-player-hand] (u/move-tiles 13 wall (list))
        new-player-hands (assoc player-hands to-player new-player-hand)]
    (assoc hand :player-hands new-player-hands, :wall new-wall)))

(defn- initial-hands-and-wall [wall]
  (reduce give-13-tiles {:wall wall :player-hands {}} winds))

(defn- new-hand [wall]
  (merge (init-hand :east) (initial-hands-and-wall wall)))

(defrecord Round [current-hand remaining-prevalent-wind])
(defn new-round [wall] (->Round (new-hand wall) winds))

(defrecord Game [current-round nb-active-players])
(def empty-game (->Game nil 0))


;;; various accessors

(defn get-round [game] (:current-round game))

(defn get-hand [game] (:current-hand (get-round game)))

(defn get-prevalent-wind [game] (first (:remaining-prevalent-wind (get-hand game))))

(defn get-turn [game] (:current-turn (get-hand game)))

(defn get-player-tiles [game player] (player (:player-hands (get-hand game))))

(defn get-player-fans [game player] (player (:fans (get-hand game))))

(defn get-player-state [game player] (get-in (get-turn game) [:player-states player]))

(defn get-player-turn [game] (:player (get-turn game)))

(defn get-not-player-turnz [game] (u/minus winds [(get-player-turn game)]))

(defn get-last-discarded [game] (last (get-in (get-hand game) [:discarded (get-player-turn game)])))

(defn get-next-player [game] (winds (mod (inc (.indexOf winds (get-player-turn game))) 4)))

(defn count-tiles-of-type [game player tile]
  (let [last-discarded   (get-last-discarded game)
        get-player-tiles #(get-player-tiles game player)
        inc-if           #(if %1 (inc %2) %2)]
    (reduce #(inc-if (= last-discarded %2) %1) 0 (get-player-tiles))))

(defn get-player-discarded [game player] (player (:discarded (get-hand game))))

(defn get-remaining-dealers [game] (:remaining-dealers (get-hand game)))

(defn get-remaining-prevalent-wind [game] (:remaining-prevalent-wind (get-round game)))

;;; various tests

(defn tile-owned? [game player tile] (some #(= % tile) (get-player-tiles game player)))

(defn can-discard? [game player] (not (= :wait-next-turn (get-player-state game player))))

(defn can-auction? [game player] (= :auction (get-player-state game player)))

(defn has-fan? [game player fan] (not (nil? (some #(= fan %1) (get-in (get-hand game) [:fans player])))))

(defn has-played-turn? [game player]
  (let [player-state (get-in game [:current-round :current-hand :current-turn :player-states player])]
    (or
      (not (nil? (some #(= % player-state) [:wait-next-turn :pung :kong :hule])))
      (and (vector? player-state) (= :chow (first player-state))))))

(defn end-turn? [game] (every? #(has-played-turn? game %) (get-not-player-turnz game)))


;;; various tiles related functions

(defn- get-char [tile position] (get (str tile) position))

(defn- char-to-int [c] (- (int c) (int \0)))

(defn family [tile] (get-char tile 1))

(defn order [tile] (char-to-int (get-char tile 2)))

(defn to-tile [family order] (keyword (str family order)))

(defn valid-chow? [game owned-tiles]
  {:pre [(set? owned-tiles)]}
  (let [last-discarded  (get-last-discarded game)
        expected-family (family last-discarded)
        order           (order last-discarded)
        proposed-chow   (conj owned-tiles last-discarded)
        to-chow         (fn [orders] (conj
                                       (reduce #(conj %1 (to-tile expected-family (+ order %2))) #{} orders)
                                       last-discarded))]
    (not (nil? (some #(= proposed-chow %1) (map to-chow [[-2 -1] [-1 1] [1 2]]))))))

(defn valid-hule?
  "TODO proper implementation"
  [hule-details] true)

(defn create-fan [type & tiles]
  (into [type] (sort tiles)))


;;; update functions

; note : all update functions end with the game parameter so that one can use ->>

(defn inc-nb-active-players [game]
  (assoc game :nb-active-players (inc (:nb-active-players game))))

(defn update-player-discarded
  "Update discarded tiles of player"
  [player new-player-discarded game]
  (assoc-in game [:current-round :current-hand :discarded player] new-player-discarded))

(defn update-player-tiles
  "Update the tiles of player"
  [player new-player-tiles game]
  (assoc-in game [:current-round :current-hand :player-hands player] new-player-tiles))

(defn update-players-states
  "Update the states of all players"
  [new-players-states game]
  (assoc-in game [:current-round :current-hand :current-turn :player-states] new-players-states))

(defn update-turn [new-turn game]
  "Replace the current turn of the game"
  (assoc-in game [:current-round :current-hand :current-turn] new-turn))

(defn update-round [new-round game]
  (assoc game :current-round new-round))

(defn draw-tile

  "2 parameters :
  - hand is :current-hand
  - to-player is player that draws tile

  1 parameter :
  - game: the game with a tile drawed by player-turn"

  ([hand to-player]
   (let [player-hands           (:player-hands hand)
         player-hand            (to-player player-hands)
         wall                   (:wall hand)
         tile-move              {:source wall :destination player-hand}
         {new-wall :source new-hand :destination} (u/move-tile tile-move)
         hand-with-updated-wall (assoc hand :wall new-wall)]
     (assoc-in hand-with-updated-wall [:player-hands to-player] new-hand)))

  ([game]
   (update-in game [:current-round :current-hand] draw-tile (get-player-turn game))))

(defn remove-from-player-tiles [player nb tile game]
  "remove nb similar tiles from player's hand"
  (let [tiles-to-remove  (into [] (take nb (repeat tile)))
        new-player-tiles (u/minus (get-player-tiles game player) tiles-to-remove)]
    (update-player-tiles player new-player-tiles game)))

(defn remove-last-discarded [game]
  (let [player-turn               (get-player-turn game)
        new-player-turn-discarded (pop (get-player-discarded game player-turn))]
    (update-player-discarded player-turn new-player-turn-discarded game)))

(defn add-player-fan [player fan game]
  (update-in game [:current-round :current-hand :fans player] conj fan))

