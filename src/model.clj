(ns majiang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def winds [:east :north :west :south ])
(def all-tiles (into [:fp :fo :fc :fb :ss :su :sa :sw ]
                     (flatten (take 4 (repeat [:b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9
                                      :c1 :c2 :c3 :c4 :c5 :c6 :c7 :c8 :c9
                                      :s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s9
                                      :we :wn :ww :ws :dr :dg :dw ])))))

(defrecord Turn [player player-states])
(defn init-turn [player-turn] (->Turn player-turn {}))

(defrecord Hand [current-turn wall player-hands discarded fans])
(defn init-hand [player-turn]
  (->Hand (init-turn player-turn) nil nil {:east [] :north [] :west [] :south []} {:east [] :north [] :west [] :south []}))

(defrecord Round [current-hand remaining-prevalent-wind])

(defrecord Game [current-round nb-active-players])
(def empty-game (->Game nil 0))


;;; various accessors

(defn get-hand [game] (:current-hand (:current-round game)))

(defn get-turn [game] (:current-turn (get-hand game)))

(defn get-player-tiles [game player] (player (:player-hands (get-hand game))))

(defn get-player-state [game player] (get-in (get-turn game) [:player-states player]))

(defn get-player-turn [game] (:player (get-turn game)))

(defn get-not-player-turnz [game] (minus winds [(get-player-turn game)]))

(defn get-last-discarded [game] (last (get-in (get-hand game) [:discarded (get-player-turn game)])))

(defn get-next-player [game] (winds (mod (inc (.indexOf winds (get-player-turn game))) 4)))

(defn count-tiles-of-type [game player tile]
  (let [last-discarded (get-last-discarded game)
          get-player-tiles #(get-player-tiles game player)
          inc-if #(if %1 (inc %2) %2)]
       (reduce #(inc-if (= last-discarded %2) %1) 0 (get-player-tiles))))

(defn get-player-discarded [game player] (player (:discarded (get-hand game))))


;;; various tests

(defn tile-owned? [game player tile] (some #(= % tile) (get-player-tiles game player)))

(defn can-discard? [game player] (not (= :wait-next-turn (get-player-state game player))))

(defn can-auction? [game player] (= :auction (get-player-state game player)))

(defn has-fan? [game player fan] (some #(= fan %1) (get-in (get-hand game) [:fans player])))

(defn has-played-turn? [game player]
  (let [player-state (get-in game [:current-round :current-hand :current-turn :player-states player])]
    (or
     (not (nil? (some #(= % player-state) [:wait-next-turn :pung :kong :hule])))
     (and (vector? player-state) ( = :chow (first player-state))))))

(defn end-turn? [game] (every? #(has-played-turn? game %) (get-not-player-turnz game)))


;;; various tiles related functions

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

(defn create-fan [type tile] [type tile])


;;; update functions

; note : all update functions end with the game parameter so that one can use ->>

(defn- inc-nb-active-players [game]
  (assoc game :nb-active-players (inc (:nb-active-players game))))

(defn- update-player-discarded
  "Update discarded tiles of player"
  [player new-player-discarded game]
  (assoc-in game [:current-round :current-hand :discarded player] new-player-discarded))

(defn- update-player-tiles
  "Update the tiles of player"
  [player new-player-tiles game]
  (assoc-in game [:current-round :current-hand :player-hands player] new-player-tiles))

(defn- update-players-states
  "Update the states of all players"
  [new-players-states game]
  (assoc-in game [:current-round :current-hand :current-turn :player-states] new-players-states))

(defn- update-turn [game turn]
  "Replace the current turn of the game"
  (assoc-in game [:current-round :current-hand :current-turn] turn))

