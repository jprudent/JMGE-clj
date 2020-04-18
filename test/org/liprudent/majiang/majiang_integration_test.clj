(ns org.liprudent.majiang.majiang-integration-test
  (:require [org.liprudent.majiang.event-sourcing :as es]
            [clojure.test :refer :all]
            [org.liprudent.majiang.model :as m]
            [org.liprudent.majiang.utils :as u]
            [org.liprudent.majiang.events :as e]
            [org.liprudent.majiang.commands :as c]))

(defn replay-all [game-id event-store]
  "Replay all events returning the resulting aggregate"
  (es/apply-events m/empty-game (flatten (:transactions (es/retrieve-event-stream event-store game-id)))))

(def game-id 11)

(defn play-game [event-store] (replay-all game-id event-store))

(defn in-game

  ([events event-store]
   (es/clear-events event-store game-id)
   (es/append-events event-store game-id
                     (es/->EventStream 0 []) events))

  ([event-store] (in-game [] event-store)))

(defn mk-crooked-wall
  "This function take multiple parameters, at least 1, at most 4.
  The 1st is wished east tiles, 2nd north, 3rd west, 4th south.
  Each of them is a vector of 13 tiles representing wished tiles.
  "
  [& wished-player-hands]
  {:pre [(every? #(= 13 (count %1)) wished-player-hands)]}

  (let [all-wished (into [] (apply concat wished-player-hands))]
    (into all-wished (u/minus m/all-tiles all-wished))))

(defn start-game-events [wall]
  [(e/->PlayerJoined game-id)
   (e/->PlayerJoined game-id)
   (e/->PlayerJoined game-id)
   (e/->PlayerJoined game-id)
   (e/->GameStarted game-id 6 6 wall)])

(deftest four-people-joining
  (let [cmd-enter (c/->NewPlayerEnter game-id)
        event-store (es/in-memory-event-store)]

    (in-game event-store)

    (is (= (m/->Game nil 0) (replay-all game-id event-store)))

    (c/handle-command cmd-enter event-store)
    (is (= (m/->Game nil 1) (replay-all game-id event-store)))

    (c/handle-command cmd-enter event-store)
    (is (= (m/->Game nil 2) (replay-all game-id event-store)))

    (c/handle-command cmd-enter event-store)
    (is (= (m/->Game nil 3) (replay-all game-id event-store)))

    (c/handle-command cmd-enter event-store)
    (let [game (replay-all game-id event-store)
          round (:current-round game)
          hand (:current-hand round)
          player-hands (:player-hands hand)
          turn (:current-turn hand)]
      (is (= 4 (:nb-active-players game)))
      (is (= [:east :north :west :south] (:remaining-prevalent-wind round)))
      (is (= (- (- 144 (* 13 4)) 1) (count (:wall hand))))
      (is (= 14 (count (:east player-hands))))
      (is (= 13 (count (:north player-hands))))
      (is (= 13 (count (:west player-hands))))
      (is (= 13 (count (:south player-hands))))
      (is (= :east (:player turn))))

    (is (thrown? Exception (c/handle-command cmd-enter event-store)))))

(deftest player-turn
  (let [wished-east-tiles [:b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4]
        crooked-wall (mk-crooked-wall wished-east-tiles)
        events (start-game-events crooked-wall)
        event-store (es/in-memory-event-store)]

    (in-game events event-store)

    (let [game (replay-all game-id event-store)
          mk-discard-cmd #(c/->DiscardTile game-id %1 (first (m/get-player-tiles game %1)))]
      ;only current player can discard
      (is (thrown? Exception (c/handle-command (mk-discard-cmd :north) event-store)))
      (is (thrown? Exception (c/handle-command (mk-discard-cmd :west) event-store)))
      (is (thrown? Exception (c/handle-command (mk-discard-cmd :south) event-store)))

      ;can discard only tiles owned
      (is (thrown? Exception (c/handle-command (c/->DiscardTile game-id :east :c5) event-store))))

    (c/handle-command (c/->DiscardTile game-id :east :c3) event-store)
    (let [game (replay-all game-id event-store)]
      (is (= [:c3] (:east (:discarded (m/get-hand game)))))
      (is (not (m/tile-owned? game :east :c3)))
      (is (= 13 (count (m/get-player-tiles game :east))))
      (is (= :wait-next-turn (m/get-player-state game :east)))
      (is (= :auction (m/get-player-state game :north)))
      (is (= :auction (m/get-player-state game :west)))
      (is (= :auction (m/get-player-state game :south))))

    ;can't discard twice
    (is (thrown? Exception (c/handle-command (c/->DiscardTile game-id :east :b2) event-store)))))



(deftest auction-pass
  (let [wished-east-tiles [:b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4]
        crooked-wall (mk-crooked-wall wished-east-tiles)
        events (conj (start-game-events crooked-wall)
                     (e/->TileDiscarded game-id :east :b1))
        event-store (es/in-memory-event-store)]

    (in-game events event-store)

    ; player who discarded can not pass
    (is (thrown? Exception (c/handle-command (c/->Pass game-id :east) event-store)))

    ; other players can pass in any order
    (c/handle-command (c/->Pass game-id :south) event-store)
    (c/handle-command (c/->Pass game-id :north) event-store)

    ; but only once
    (is (thrown? Exception (c/handle-command (c/->Pass game-id :north) event-store)))

    (let [game (replay-all game-id event-store)]
      (= (is (m/get-player-state game :north) :wait-next-turn)))))

(deftest auction-chow
  (let [wished-east-tiles [:b1 :b2, :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4]
        wished-north-tiles [:b1 #__ :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4 :c5]
        wished-west-tiles [:b1 :b2, :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4]
        crooked-wall (mk-crooked-wall wished-east-tiles wished-north-tiles wished-west-tiles)
        events (conj (start-game-events crooked-wall)
                     (e/->TileDiscarded game-id :east :b2))
        event-store (es/in-memory-event-store)]

    (in-game events event-store)

    ; player who discarded can not chow
    (is (thrown? Exception (c/handle-command (c/->Chow game-id :east #{:b1 :b3}) event-store)))

    ; only player on the right can chow
    (is (thrown? Exception (c/handle-command (c/->Chow game-id :west #{:b1 :b3}) event-store)))

    ; north can Chow but only with a valid set of tiles
    (is (thrown? Exception (c/handle-command (c/->Chow game-id :north #{:b5 :b6}) event-store)))
    (c/handle-command (c/->Chow game-id :north #{:b3 :b4}) event-store)

    (let [game (replay-all game-id event-store)]
      (is (= [:chow #{:b3 :b4}] (m/get-player-state game :north))))))

(deftest auction-pung
  (let [wished-east-tiles [:b1 :b2 #_____ :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4]
        wished-north-tiles [:b1 #_________ :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4 :c5]
        wished-west-tiles [:b1 :b2 :b2 :b2 #_____ :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4]
        crooked-wall (mk-crooked-wall wished-east-tiles wished-north-tiles wished-west-tiles)
        events (conj (start-game-events crooked-wall)
                     (e/->TileDiscarded game-id :east :b2))
        event-store (es/in-memory-event-store)]

    (in-game events event-store)

    ; player who discarded can not pung
    (is (thrown? Exception (c/handle-command (c/->Pung game-id :east) event-store)))

    ; must have appropriate tiles to pung
    (is (thrown? Exception (c/handle-command (c/->Pung game-id :north) event-store)))

    ; any player can pung
    (c/handle-command (c/->Pung game-id :west) event-store)

    (let [game (replay-all game-id event-store)]
      (is (= :pung (m/get-player-state game :west))))))

(deftest auction-kong
  (let [wished-east-tiles [:b1 :b2 #_____ :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4]
        wished-north-tiles [:b1 #_________ :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4 :c5]
        wished-west-tiles [:b1 :b2 :b2 :b2 #_____ :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4]
        crooked-wall (mk-crooked-wall wished-east-tiles wished-north-tiles wished-west-tiles)
        events (conj (start-game-events crooked-wall)
                     (e/->TileDiscarded game-id :east :b2))
        event-store (es/in-memory-event-store)]

    (in-game events event-store)

    ; player who discarded can not
    (is (thrown? Exception (c/handle-command (c/->Kong game-id :east) event-store)))

    ; must have appropriate tiles
    (is (thrown? Exception (c/handle-command (c/->Kong game-id :north) event-store)))

    ; not next player can kong
    (c/handle-command (c/->Kong game-id :west) event-store)

    (let [game (replay-all game-id event-store)]
      (is (= :kong (m/get-player-state game :west))))))

(deftest auction-all-passed
  (let [events (conj (start-game-events m/all-tiles)
                     (e/->TileDiscarded game-id :east :b1))
        event-store (es/in-memory-event-store)]

    (in-game events event-store)

    (c/handle-command (c/->Pass game-id :north) event-store)
    (c/handle-command (c/->Pass game-id :west) event-store)
    (c/handle-command (c/->Pass game-id :south) event-store)

    ;a new turn is launched
    (let [game (replay-all game-id event-store)]
      (is (= :north (m/get-player-turn game)))
      (is (= 14 (count (m/get-player-tiles game :north))))
      (is (m/can-discard? game :north))
      (is (every? #(not (m/can-auction? game %)) (u/minus m/winds [:north]))))))

(deftest auction-chow-pung-compete
  (let [wished-east-tiles [:b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4]
        wished-north-tiles [:b1 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4 :c5 :c6]
        wished-west-tiles [:b3 :b3 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4 :c5 :c6]
        crooked-wall (mk-crooked-wall wished-east-tiles wished-north-tiles wished-west-tiles)
        events (conj (start-game-events crooked-wall)
                     (e/->TileDiscarded game-id :east :b3))
        event-store (es/in-memory-event-store)]

    (in-game events event-store)

    (c/handle-command (c/->Chow game-id :north #{:b4 :b5}) event-store)
    (c/handle-command (c/->Pung game-id :west) event-store)
    (c/handle-command (c/->Pass game-id :south) event-store)

    ;a new turn is launched, pung claim win
    (let [game (replay-all game-id event-store)]
      (is (= :west (m/get-player-turn game)))
      (is (= 11 (count (m/get-player-tiles game :west))))
      (is (not (m/tile-owned? game :west :b3)))
      (is (m/has-fan? game :west (m/create-fan :pung :b3)))
      (is (m/can-discard? game :west))
      (is (every? #(not (m/can-auction? game %)) (u/minus m/winds [:west]))))))

(deftest auction-chow-kong-compete
  (let [wished-east-tiles [:b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4]
        wished-north-tiles [:b1 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4 :c5 :c6]
        wished-west-tiles [:b3 :b3 :b3 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4 :c5 :c6]
        crooked-wall (mk-crooked-wall wished-east-tiles wished-north-tiles wished-west-tiles)
        events (conj (start-game-events crooked-wall)
                     (e/->TileDiscarded game-id :east :b3))
        event-store (es/in-memory-event-store)]

    (in-game events event-store)

    (c/handle-command (c/->Chow game-id :north #{:b4 :b5}) event-store)
    (c/handle-command (c/->Kong game-id :west) event-store)
    (c/handle-command (c/->Pass game-id :south) event-store)

    ;a new turn is launched, pung claim win
    (let [game (replay-all game-id event-store)]
      (is (= :west (m/get-player-turn game)))
      (is (= 11 (count (m/get-player-tiles game :west))))
      (is (not (m/tile-owned? game :west :b3)))
      (is (m/has-fan? game :west (m/create-fan :kong :b3)))
      (is (m/can-discard? game :west))
      (is (every? #(not (m/can-auction? game %)) (u/minus m/winds [:west]))))))


(deftest auction-chow-only-compete
  (let [wished-east-tiles [:b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4]
        wished-north-tiles [:b1 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4 :c5 :c6]
        wished-west-tiles [:b3 :b3 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4 :c5 :c6]
        crooked-wall (mk-crooked-wall wished-east-tiles wished-north-tiles wished-west-tiles)
        events (conj (start-game-events crooked-wall)
                     (e/->TileDiscarded game-id :east :b3))
        event-store (es/in-memory-event-store)]

    (in-game events event-store)

    (c/handle-command (c/->Chow game-id :north #{:b4 :b5}) event-store)
    (c/handle-command (c/->Pass game-id :west) event-store)
    (c/handle-command (c/->Pass game-id :south) event-store)

    ;a new turn is launched, pung claim win
    (let [game (replay-all game-id event-store)]
      (is (= :north (m/get-player-turn game)))
      (is (= 11 (count (m/get-player-tiles game :north))))
      (is (not-any? #(m/tile-owned? game :north %) (list :b3 :b4 :b5)))
      (is (m/has-fan? game :north (m/create-fan :chow :b3 :b4 :b5)))
      (is (m/can-discard? game :north))
      (is (every? #(not (m/can-auction? game %)) (u/minus m/winds [:north]))))))



(comment deftest auction-chow-hule-compete
         (let [wished-east-tiles [:b3 :s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s9 :s1 :s2 :s3]
               wished-north-tiles [:b4 :b5 :s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s9 :s1 :s2]
               wished-west-tiles [:b3 :b3 :b5 :b5 :b5 :b6 :b6 :b6 :b7 :b7 :b7 :b8 :b8]
               crooked-wall (mk-crooked-wall wished-east-tiles wished-north-tiles wished-west-tiles)
               events (conj (start-game-events crooked-wall)
                            (->TileDiscarded game-id :east :b3))]

           (in-game events)

           (handle-command (->Chow game-id :north #{:b4 :b5}) event-store)
           (handle-command (->Pass game-id :south) event-store)
           (handle-command (->Hule game-id :west) event-store)

           ;a new hand is launched
           (let [game (replay-all game-id)
                 round (get-round game)
                 hand (get-hand game)]
             (is (= :north (get-player-turn game)) "hand wind changed")
             (is (= [:east :north :west :south] (:remaining-prevalent-wind round)))
             (is (= [:north :west :south] (:remaining-prevalent-wind hand)))
             (is (= (- (- 144 (* 13 4)) 1) (count (:wall hand))))
             (is (= 14 (count (get-player-tiles :north game))))
             (is (= 13 (count (get-player-tiles :west game))))
             (is (= 13 (count (get-player-tiles :south game))))
             (is (= 13 (count (get-player-tiles :east game)))))))

