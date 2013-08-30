(ns majiang-test
  (:use majiang clojure.test))

(defn replay-all [aggregate-id]
  "Replay all events returning the resulting aggregate"
  (apply-events empty-game (flatten (:transactions (retrieve-event-stream in-memory-event-store aggregate-id)))))

(def aggregate-id 11)

(deftest four-people-joining
  (let [cmd-enter (->NewPlayerEnter aggregate-id)]

    (clear-events in-memory-event-store aggregate-id)
    (is (= (->Game nil 0) (replay-all aggregate-id)))

    (handle-command cmd-enter in-memory-event-store)
    (is (= (->Game nil 1) (replay-all aggregate-id)))

    (handle-command cmd-enter in-memory-event-store)
    (is (= (->Game nil 2) (replay-all aggregate-id)))

    (handle-command cmd-enter in-memory-event-store)
    (is (= (->Game nil 3) (replay-all aggregate-id)))

    (handle-command cmd-enter in-memory-event-store)
    (let [game (replay-all aggregate-id)
          round (:current-round game)
          hand (:current-hand round)
          player-hands (:player-hands hand)
          turn (:current-turn hand)]
      (is (= 4 (:nb-active-players game)))
      (is (= [:east :north :west :south ] (:remaining-prevalent-wind round)))
      (is (= (- (- 144 (* 13 4)) 1) (count (:wall hand))))
      (is (= 14 (count (:east player-hands))))
      (is (= 13 (count (:north player-hands))))
      (is (= 13 (count (:west player-hands))))
      (is (= 13 (count (:south player-hands))))
      (is (= :east (:player turn))))

    (is (thrown? Exception (handle-command cmd-enter in-memory-event-store)))))

(deftest player-turn
  (let [some-tiles [:b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4 ]
        wished-east-tiles (conj some-tiles :fp )
        crooked-wall (into wished-east-tiles (minus all-tiles wished-east-tiles))
        events [(->PlayerJoined aggregate-id)
                (->PlayerJoined aggregate-id)
                (->PlayerJoined aggregate-id)
                (->PlayerJoined aggregate-id)
                (->GameStarted aggregate-id 6 6 crooked-wall)]]

    (clear-events in-memory-event-store aggregate-id)
    (append-events in-memory-event-store aggregate-id (->EventStream 0 []) events)

    (let [game (replay-all aggregate-id)
          mk-discard-cmd #(->DiscardTile %1 (first (get-player-tiles game %1)))]
      ;only current player can discard
      (is (thrown? Exception (handle-command (mk-discard-cmd :north ) in-memory-event-store)))
      (is (thrown? Exception (handle-command (mk-discard-cmd :west ) in-memory-event-store)))
      (is (thrown? Exception (handle-command (mk-discard-cmd :south ) in-memory-event-store)))

      ;can discard only tiles owned
      (is (thrown? Exception (handle-command (->DiscardTile :east :c5 ) in-memory-event-store))))

    (handle-command (->DiscardTile aggregate-id :east :b1 ) in-memory-event-store)
    (let [game (replay-all aggregate-id)]
      (is (= [:b1 ] (:east (:discarded (get-hand game)))))
      (is (not (tile-owned? game :east :b1 )))
      (is (= 13 (count (get-player-tiles game :east ))))
      (is (= :wait-next-turn (get-player-state game :east))))

    ;can't discard twice
    (is (thrown? Exception (handle-command (->DiscardTile aggregate-id :east :b2 ) in-memory-event-store)))))



(deftest auction-pass
  (let [some-tiles [:b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3 :c4 ]
        wished-east-tiles (conj some-tiles :fp )
        crooked-wall (into wished-east-tiles (minus all-tiles wished-east-tiles))
        events [(->PlayerJoined aggregate-id)
                (->PlayerJoined aggregate-id)
                (->PlayerJoined aggregate-id)
                (->PlayerJoined aggregate-id)
                (->GameStarted aggregate-id 6 6 crooked-wall)
                (->TileDiscarded aggregate-id :east :b1)]]

    (clear-events in-memory-event-store aggregate-id)
    (append-events in-memory-event-store aggregate-id (->EventStream 0 []) events)
    (replay-all aggregate-id)
    ; player who discarded can not pass
    (is (thrown? Exception (handle-command (->Pass :east ) in-memory-event-store)))

    ; other players can pass
    (handle-command (->Pass aggregate-id :north) in-memory-event-store))
    ; but only once
    (is (thrown? Exception (handle-command (->Pass :north ) in-memory-event-store)))

    (let [game (replay-all aggregate-id)]
      (= (is (get-player-state game :north) :wait-next-turn))))


(with-test-out (run-tests))








