(ns org.liprudent.majiang.majiang-unit-test
  (:require [org.liprudent.majiang.model :as m]
            [clojure.test :refer :all]
            [org.liprudent.majiang.events :as e]))

(deftest test-valid-chow?
  (let [game {:current-round {:current-hand {:discarded {:west [:b1 :b3 ]}
                                             :current-turn {:player :west}}}}]
    (is (m/valid-chow? game #{:b1 :b2 }))
    (is (m/valid-chow? game #{:b4 :b2 }))
    (is (m/valid-chow? game #{:b4 :b5 }))
    (is (not (m/valid-chow? game #{})))
    (is (not (m/valid-chow? game #{:b2 })))))

(deftest test-get-next-player
  (let [mk-game #(let [] {:current-round {:current-hand {:current-turn {:player %}}}})]
    (is (= :north (m/get-next-player (mk-game :east ))))
    (is (= :west (m/get-next-player (mk-game :north ))))
    (is (= :south (m/get-next-player (mk-game :west ))))
    (is (= :east (m/get-next-player (mk-game :south ))))))

(deftest test-has-played-turn?
  (let [mk-game #(let [] {:current-round {:current-hand {:current-turn {:player-states {:east %}}}}})]
    (is (m/has-played-turn? (mk-game :wait-next-turn ) :east ))
    (is (m/has-played-turn? (mk-game :pung ) :east ))
    (is (m/has-played-turn? (mk-game :kong ) :east ))
    (is (m/has-played-turn? (mk-game :hule ) :east ))
    (is (m/has-played-turn? (mk-game [:chow #{:b1 :b2 }]) :east ))
    (is (not (m/has-played-turn? (mk-game :foo ) :east )))))

(deftest test-can-auction?
  (let [game {:current-round {:current-hand {:current-turn {:player-states {:south :auction,
                                                                            :west :auction,
                                                                            :north :auction,
                                                                            :east :wait-next-turn}}}}}]
    (is (m/can-auction? game :south ))
    (is (m/can-auction? game :west ))
    (is (m/can-auction? game :north ))
    (is (not (m/can-auction? game :east )))))

(deftest test-end-turn?
  (let [game {:current-round {:current-hand {:current-turn {:player :east
                                                            :player-states {:south :auction,
                                                                            :west :auction,
                                                                            :north :auction,
                                                                            :east :wait-next-turn}}}}}]
    (is (not (m/end-turn? game)))))

(deftest test-create-fan
  (is (= [:pung :b1 ] (m/create-fan :pung :b1 ))))

(deftest test-greater-auction
  (let [create (fn [auction] [nil auction])
        [pass chow pung kong] (map create (list :wait-next-turn :chow :pung :kong ))]
    (is (= pass (e/greater-auction pass pass)))
    (is (= chow (e/greater-auction chow pass)))
    (is (= chow (e/greater-auction pass chow)))
    (is (= chow (e/greater-auction chow chow)))
    (is (= pung (e/greater-auction pung pass)))
    (is (= pung (e/greater-auction pass pung)))
    (is (= pung (e/greater-auction chow pung)))
    (is (= pung (e/greater-auction pung chow)))
    (is (= kong (e/greater-auction kong pass)))
    (is (= kong (e/greater-auction chow kong)))))


(deftest test-last-hand?
  (let [create (fn [remaining-dealers]
                 {:current-round {:current-hand {:remaining-dealers remaining-dealers}}})]
    (is (not (e/last-hand? (create m/winds))))
    (is (not (e/last-hand? (create (subvec m/winds 1)))))
    (is (not (e/last-hand? (create (subvec m/winds 2)))))
    (is (e/last-hand? (create (subvec m/winds 3))))))

(deftest test-last-round?
  (let [create (fn [remaining-prevalent-winds]
                 {:current-round {:remaining-prevalent-wind remaining-prevalent-winds}})]
    (is (not (e/last-round? (create m/winds))))
    (is (not (e/last-round? (create (subvec m/winds 1)))))
    (is (not (e/last-round? (create (subvec m/winds 2)))))
    (is (e/last-round? (create (subvec m/winds 3))))))