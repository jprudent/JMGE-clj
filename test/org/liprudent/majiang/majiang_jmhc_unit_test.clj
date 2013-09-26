(ns org.liprudent.majiang.majiang-jmhc-unit-test
  (:use org.liprudent.majiang.core clojure.test))

(deftest test-hule-details
  (let [game {:current-round {:current-hand {:discarded {:west [:b1 :b3 ]}
                                             :current-turn {:player :west}}}}]
    (println "je suis lance")
    (is (= (hule-details game :east) "ERROR"))))
