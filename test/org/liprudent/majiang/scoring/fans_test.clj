(ns org.liprudent.majiang.scoring.fans-test
  (:require [clojure.test :refer :all]
            [org.liprudent.majiang.scoring.fans :as sut]
            [org.liprudent.majiang.scoring.sort :as sort]))

(deftest big-three-dragons-test
  (let [pungs [[:pung :dr] [:pung :dg] [:kong :dw]]]
    (is (= #{(sort/sort-fans pungs)}
           (sut/big-three-dragons {:pungs-or-kongs pungs}))))
  
  (let [pungs [[:pung :we] [:pung :dg] [:kong :dw]]]
    (is (empty? (sut/big-three-dragons {:pungs-or-kongs pungs})))))

(deftest big-four-winds-test
  (let [pungs [[:pung :wn] [:pung :we] [:pung :ws] [:pung :ww]]]
    (is (= #{(sort/sort-fans pungs)}
           (sut/big-four-winds {:pungs-or-kongs pungs}))))
  (let [pungs [[:pung :wn] [:pung :we] [:pung :ws]]]
    (is (empty? (sut/big-four-winds {:pungs-or-kongs pungs}))
        "missing one"))

  (let [pungs [[:pung :wn] [:pung :we] [:pung :ws] [:pung :dw]]]
    (is (empty? (sut/big-four-winds {:pungs-or-kongs pungs})))))