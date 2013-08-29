(ns majiang-test
 (:use majiang clojure.test))

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
          (is (= [:east :north :west :south] (:remaining-prevalent-wind round)))
          (is (= (- 144 (* 13 4)) (count (:wall hand))))
          (is (= 13 (count (:east player-hands))))
          (is (= 13 (count (:north player-hands))))
          (is (= 13 (count (:west player-hands))))
          (is (= 13 (count (:south player-hands))))
          (is (= :east (:player turn))))
    (is (thrown? Exception (handle-command cmd-enter in-memory-event-store)))))

(with-test-out (run-tests))

(println "assert" *assert*)



