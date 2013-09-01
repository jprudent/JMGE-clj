(ns majiang-unit-test
  (:use majiang clojure.test))

(deftest test-valid-chow?
  (let [game {:current-round
              {:current-hand
               {:discarded {:west [:b1 :b3]}
                :current-turn {:player :west}}}}]
    (is (valid-chow? game #{:b1 :b2}))
    (is (valid-chow? game #{:b4 :b2}))
    (is (valid-chow? game #{:b4 :b5}))
    (is (not (valid-chow? game #{})))
    (is (not (valid-chow? game #{:b2})))))

(deftest test-get-next-player
  (let [mk-game #(let [] {:current-round
                    {:current-hand
                       {:current-turn {:player %}}}})]
    (is (= :north (get-next-player (mk-game :east))))
    (is (= :west (get-next-player (mk-game :north))))
    (is (= :south (get-next-player (mk-game :west))))
    (is (= :east (get-next-player (mk-game :south))))))

(deftest test-has-played-turn?
  (let [mk-game #(let [] {:current-round
                    {:current-hand
                       {:current-turn {:player-states {:east %}}}}})]
    (is (has-played-turn? (mk-game :wait-next-turn) :east))
    (is (has-played-turn? (mk-game :pung) :east))
    (is (has-played-turn? (mk-game :kong) :east))
    (is (has-played-turn? (mk-game :hule) :east))
    (is (has-played-turn? (mk-game [:chow #{:b1 :b2}]) :east))
    (is (not (has-played-turn? (mk-game :foo) :east)))))
