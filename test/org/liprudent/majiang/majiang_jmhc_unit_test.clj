(ns org.liprudent.majiang.majiang-jmhc-unit-test
  (:use org.liprudent.majiang.core clojure.test))

(def eg-game {:current-round {:current-hand {:player-hands {:west [:b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9 :c1 :c2 :c3]}
                                            :fans {:west [(create-fan :chow :b1 :b2 :b3)
                                                          (create-fan :pung :c1)
                                                          (create-fan :kong :s1)]}
                                                  :discarded {:east [:b1 :b3 ]}
                                                  :current-turn {:player :east}}}})
(deftest test-hule-details
    (println "je suis lance")
    (is (= (hule-details eg-game :west) "ERROR")))

(deftest test-to-json
  (let [json (to-json eg-game :west)]
    (is (= json (str "{"
    "\"concealed\":\"b1 b2 b3 b4 b5 b6 b7 b8 b9 c1 c2 c3\","
    "\"melded\":\"b1-b2-b3 c1-c1-c1 s1-s1-s1-s1\","
    "\"winning_tile\":\"b3\""
    "}")))))
