(ns org.liprudent.majiang.majiang-unit-test
  (:use org.liprudent.majiang.core clojure.test))

(deftest test-valid-chow?
  (let [game {:current-round {:current-hand {:discarded {:west [:b1 :b3 ]}
                                             :current-turn {:player :west}}}}]
    (is (valid-chow? game #{:b1 :b2 }))
    (is (valid-chow? game #{:b4 :b2 }))
    (is (valid-chow? game #{:b4 :b5 }))
    (is (not (valid-chow? game #{})))
    (is (not (valid-chow? game #{:b2 })))))

(deftest test-get-next-player
  (let [mk-game #(let [] {:current-round {:current-hand {:current-turn {:player %}}}})]
    (is (= :north (get-next-player (mk-game :east ))))
    (is (= :west (get-next-player (mk-game :north ))))
    (is (= :south (get-next-player (mk-game :west ))))
    (is (= :east (get-next-player (mk-game :south ))))))

(deftest test-has-played-turn?
  (let [mk-game #(let [] {:current-round {:current-hand {:current-turn {:player-states {:east %}}}}})]
    (is (has-played-turn? (mk-game :wait-next-turn ) :east ))
    (is (has-played-turn? (mk-game :pung ) :east ))
    (is (has-played-turn? (mk-game :kong ) :east ))
    (is (has-played-turn? (mk-game :hule ) :east ))
    (is (has-played-turn? (mk-game [:chow #{:b1 :b2 }]) :east ))
    (is (not (has-played-turn? (mk-game :foo ) :east )))))

(deftest test-can-auction?
  (let [game {:current-round {:current-hand {:current-turn {:player-states {:south :auction,
                                                                            :west :auction,
                                                                            :north :auction,
                                                                            :east :wait-next-turn}}}}}]
    (is (can-auction? game :south ))
    (is (can-auction? game :west ))
    (is (can-auction? game :north ))
    (is (not (can-auction? game :east )))))

(deftest test-end-turn?
  (let [game {:current-round {:current-hand {:current-turn {:player :east
                                                            :player-states {:south :auction,
                                                                            :west :auction,
                                                                            :north :auction,
                                                                            :east :wait-next-turn}}}}}]
    (is (not (end-turn? game)))))

(deftest test-create-fan
  (is (= [:pung :b1 ] (create-fan :pung :b1 ))))

(deftest test-greater-auction
  (let [create (fn [auction] [nil auction])
        [pass chow pung kong] (map create (list :wait-next-turn :chow :pung :kong ))]
    (is (= pass (greater-auction pass pass)))
    (is (= chow (greater-auction chow pass)))
    (is (= chow (greater-auction pass chow)))
    (is (= chow (greater-auction chow chow)))
    (is (= pung (greater-auction pung pass)))
    (is (= pung (greater-auction pass pung)))
    (is (= pung (greater-auction chow pung)))
    (is (= pung (greater-auction pung chow)))
    (is (= kong (greater-auction kong pass)))
    (is (= kong (greater-auction chow kong)))))

