(ns org.liprudent.majiang.scoring.core-test
  (:require [clojure.test :refer :all])
  (:require [org.liprudent.majiang.scoring.core :as sut]
            [org.liprudent.majiang.model :as m]
            [clojure.set :as set]
            [org.liprudent.majiang.utils :as u]))

(deftest valid-pattern?-test
  (is (sut/valid-pattern? [:b5 :b5]))
  (is (sut/valid-pattern? [:b6 :b5 :b5 :b5 :b6]))
  (is (sut/valid-pattern? [:b1 :b1 :b1 :b5 :b5]))
  (is (sut/valid-pattern? [:b1 :b1 :b1 :b2 :b3]))
  (is (sut/valid-pattern? [:b1 :b1 :b1 :b2 :b3 :b2 :b2 :b2]))
  (is (sut/valid-pattern? [:b1 :b2 :b3 :b1 :b2 :b3 :b1 :b2 :b3 :b4 :b4])))

(deftest find-valid-pattern-test
  (is (= #{[[:pung :b9]
            [:chow :b1 :b2 :b3]
            [:chow :b4 :b5 :b6]
            [:chow :b6 :b7 :b8]
            [:pair :b1]]}
         (sut/find-valid-patterns [:b1 :b1 :b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9 :b9 :b9 :b6])))
  (is (= #{[[:pung :s1]
            [:chow :b1 :b2 :b3]
            [:chow :b1 :b2 :b3]
            [:chow :b1 :b2 :b3]
            [:pair :s2]]
           [[:pung :b1]
            [:pung :b2]
            [:pung :b3]
            [:pung :s1]
            [:pair :s2]]}
         (sut/find-valid-patterns [:b1 :b1 :b1
                                   :b2 :b2 :b2
                                   :b3 :b3 :b3
                                   :s1 :s1 :s1
                                   :s2 :s2]))))

(defn having-pungs-or-kongs
  [& tiles]
  (fn [{:keys [pungs-or-kongs]}]
    (= (set tiles)
       (set/intersection (set (map second pungs-or-kongs))
                         (set tiles)))))

(defn only-tiles
  [& allowed-tiles]
  (fn [{:keys [all-tiles]}]
    (= (set all-tiles)
       (set/intersection (set allowed-tiles)
                         (set all-tiles)))))

(defn fully-concealed?
  [{:keys [hand]}]
  (= 5 (count hand)))

(defn families
  [tiles]
  (map m/family tiles))

(defn full-flush?
  [{:keys [all-tiles]}]
  (= 1 (count (set (families all-tiles)))))

(defn min-occurence
  [tiles min-occurences]
  (let [freq (frequencies (map m/order tiles))]
    (->> (merge-with - freq min-occurences)
         (vals)
         (every? nat-int?))))
(defn nine-gates
  [{:keys [all-tiles] :as context}]
  (and (fully-concealed? context)
       (full-flush? context)
       (min-occurence all-tiles
                      {1 3, 2 1, 3 1, 4 1, 5 1, 6 1, 7 1, 8 1, 9 3})))


(def fans
  {:big-four-winds
   {:name        "Big four winds"
    :description "Pungs or Kongs of all four Wind Tiles"
    :points      88
    :predicate   (having-pungs-or-kongs :we :ws :ww :wn)
    :exclusions  [:big-three-winds
                  :all-pungs
                  :prevalent-wind
                  :seat-wind
                  :pung-of-terminals-or-honors]}
   :big-three-dragons
   {:name        "Big three dragons"
    :description "Pungs or Kongs of all three Dragon tiles"
    :points      88
    :predicate   (having-pungs-or-kongs :dr :dw :dg)
    :exclusions  [:two-dragons
                  :dragon-pung]}

   :all-green
   {:name        "All green"
    :description "Hand is composed entirely of any of the 2, 3, 4, 6, 8 of Bamboo and Green Dragon"
    :points      88
    :predicate   (only-tiles :b2 :b3 :b4 :b6 :b8 :dg)}

   :nine-gates
   {:name        "Nine gates"
    :description "Holding the 1, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 9 tiles in one suit, creating the nine-sided wait of 1, 2, 3, 4, 5, 6, 7, 8, 9."
    :points      88
    :predicate   nine-gates
    :exclusions  [:full-flush
                  :concealed-hand
                  :pung-of-terminals-or-honors]}})



(def pair? (comp #{:pair} first))
(def pung? (comp #{:pung} first))
(def kong? (comp #{:kong} first))
(def chow? (comp #{:chow} first))

(defn all-pungs
  [game]
  (into (filter pung? (:hand game))
        (filter pung? (:fans game))))

(defn all-kongs
  [game]
  (into (filter kong? (:fans game))
        (:concealed-kongs game)))

(defn all-pairs
  [game]
  (filter pair? (:hand game)))

(defn all-chows
  [game]
  (into (filter chow? (:hand game))
        (filter chow? (:fans game))))

(defn fan->tiles
  [[fan-type t1 t2 t3]]
  (case fan-type
    :pung [t1 t1 t1]
    :chow [t1 t2 t3]
    :pair [t1 t1]
    :kong [t1 t1 t1 t1]))

(defn all-tiles
  [& fans]
  (sort (mapcat fan->tiles (reduce into fans))))

(defn scoring
  [game]
  (doall (for [hand (sut/find-valid-patterns (:hand game))
               :let [game           (assoc game :hand hand)
                     pungs          (all-pungs game)
                     kongs          (all-kongs game)
                     pungs-or-kongs (into pungs kongs)
                     pairs          (all-pairs game)
                     chows          (all-chows game)
                     all-tiles      (all-tiles pungs-or-kongs pairs chows)
                     context        {:game           game
                                     :hand           hand
                                     :pungs          pungs
                                     :kongs          kongs
                                     :pairs          pairs
                                     :pungs-or-kongs pungs-or-kongs
                                     :chows          chows
                                     :all-tiles      all-tiles}]]
           (doall (for [fan fans
                        :let [[_k {:keys [predicate] :as r}] fan]
                        :when (predicate context)]
                    r)))))

(deftest scoring-test

  (is (= [[(:big-four-winds fans)]]
         (scoring
           {:hand           [:ws :ws :ws
                             :ww :ww :ww
                             :wn :wn :wn
                             :dr :dr]
            :fans           [[:pung :we]]
            :out            [:ws :discarded]
            :wind           :we
            :prevalent-wind :we
            :seat-wind      :ws})))

  (is (= [[(:big-three-dragons fans)]]
         (scoring
           {:hand           [:dr :dw :dg
                             :dr :dw :dg
                             :dr :dw :dg
                             :s1 :s1]
            :fans           [[:pung :we]]
            :out            [:s1 :discarded]
            :wind           :we
            :prevalent-wind :we
            :seat-wind      :ws})))

  (is (= [[(:all-green fans)]
          [(:all-green fans)]]
         (scoring
           {:hand           [:b2 :b3 :b4
                             :b2 :b3 :b4
                             :b2 :b3 :b4
                             :dg :dg]
            :fans           [[:pung :b8]]
            :out            [:dg :discarded]
            :wind           :we
            :prevalent-wind :we
            :seat-wind      :ws})))

  (is (= [[(:nine-gates fans)]]
         (scoring
           {:hand           [:b1 :b1 :b1
                             :b2 :b3 :b4 :b5 :b6 :b7 :b8
                             :b9 :b9 :b9
                             :b6]
            :fans           [[:pung :b8]]
            :out            [:b6 :discarded]
            :wind           :we
            :prevalent-wind :we
            :seat-wind      :ws}))))
