(ns org.liprudent.majiang.scoring.core
  (:require [clojure.math.combinatorics :as combo]
            [org.liprudent.majiang.scoring.fans :as fans]
            [org.liprudent.majiang.scoring.sort :as sort]
            [org.liprudent.majiang.model :as m]))

(def all-suits
  (set (for [suit-number (partition 3 1 (range 1 10))
             family      [\b \c \s]]
         (map #(keyword (str family %)) suit-number))))

(defn pung-freq? [[t1 t2 t3 & others]]
  (and (nil? others) (= t1 t2 t3)))

(defn chow-freq? [tiles]
  (and (= 3 (count tiles)) (all-suits tiles)))

(defn pair-freq? [[t1 t2 & others]]
  (and (nil? others) (= t1 t2)))

(defn has-enough-tiles?
  [freq]
  (every? nat-int? (vals freq)))

(defn fan-freqs->fan
  [pattern]
  (for [fan pattern]
    (cond
      (= 3 (count fan))
      (reduce-kv (fn [fan tile nb]
                   (assert (= 1 nb))
                   (conj fan tile))
                 [:chow]
                 fan)

      (= 1 (count fan))
      (let [[tile nb] (first fan)]
        [(case nb, 2 :pair, 3 :pung) tile]))))

(defn find-the-pair [freqs]
  (let [[[tile nb] :as remaining] (filter (fn [[_k v]] (pos-int? v)) freqs)]
    (when (and (= 1 (count remaining)) (= 2 nb))
      {tile nb})))

(defn fan-combinations
  "nb-fan-target doesn't include the pair"
  [nb-fan-target
   hand-freqs
   [fan-freq & other-fan-freqs :as fan-freqs]]
  (if (and fan-freq (pos-int? nb-fan-target)
           ;; it will never make it otherwise
           ;; multiply by 2 for the 7 pairs worst case
           #_(>= (* 2 (count fan-freqs)) nb-fan-target))
    (mapcat
      identity
      ;; either try to use fan-freq
      [(let [new-hand-freq (merge-with - hand-freqs fan-freq)
             new-fan-freqs (if (or (= 3 (count fan-freq))
                                   (= 2 (-> fan-freq first second)))
                             ;; chow is reusable
                             fan-freqs
                             other-fan-freqs)]
         (when (has-enough-tiles? new-hand-freq)
           (let [[r1 r2] (fan-combinations
                           (dec nb-fan-target)
                           new-hand-freq
                           new-fan-freqs)]
             (cond-> []
                     r1 (conj (conj r1 fan-freq))
                     r2 (conj (conj r2 fan-freq))))))
       ;; either skip it
       (fan-combinations nb-fan-target hand-freqs other-fan-freqs)])
    (when-let [pair (find-the-pair hand-freqs)]
      [[pair]])))


(defn pattern-finder
  [fan-size valid-fan-pred]
  (fn [tiles]
    (let [fan-freqs     (->> (combo/combinations tiles fan-size)
                             (filter valid-fan-pred)
                             (map frequencies))
          tile-freqs    (frequencies tiles)
          nb-fan-target (let [x (/ (count tiles) fan-size)]
                          (if (int? x) (dec x) (int x)))]
      (->> (fan-combinations nb-fan-target tile-freqs fan-freqs)
           (map fan-freqs->fan)
           (map sort/sort-fans)
           (not-empty)))))

(def classic-hand (pattern-finder 3 (some-fn pung-freq? chow-freq?)))
(def seven-pairs-hand (pattern-finder 2 pair-freq?))

(defn thirteen-orphans
  [tiles]
  (when (= #{:b1 :b9 :c1 :c9 :s1 :s9 :dr :dg :dw :we :ws :ww :wn} (set tiles))
    [[(apply vector :thirteen-orphans tiles)]]))

(defn knitted-hand
  [tiles]
  (let [{:keys [:honors :suits]} (group-by #(if (m/honors %) :honors :suits) tiles)
        {chars \c, bamboos \b, stones \s} (group-by m/family suits)
        chars   (map m/order chars)
        bamboos (map m/order bamboos)
        stones  (map m/order stones)
        g1      (set (range 1 10 3))
        g2      (set (range 2 10 3))
        g3      (set (range 3 10 3))]
    (when (and (= m/honors (set honors))
               (or (and (every? g1 bamboos) (every? g2 chars) (every? g3 stones))
                   (and (every? g1 bamboos) (every? g3 chars) (every? g2 stones))

                   (and (every? g2 bamboos) (every? g1 chars) (every? g3 stones))
                   (and (every? g2 bamboos) (every? g3 chars) (every? g1 stones))

                   (and (every? g3 bamboos) (every? g2 chars) (every? g1 stones))
                   (and (every? g3 bamboos) (every? g1 chars) (every? g2 stones))))
      [[(apply vector :knitted (sort/sort-tiles tiles))]])))

(defn find-valid-patterns
  [tiles]
  (reduce into #{} [(classic-hand tiles)
                    (seven-pairs-hand tiles)
                    (thirteen-orphans tiles)
                    (knitted-hand tiles)]))

(defn valid-pattern?
  "returns true if tiles can be arranged as a set of valid patterns"
  [tiles]
  (some? (find-valid-patterns tiles)))

(def pair? (comp #{:pair} first))
(def pung? (comp #{:pung} first))
(def kong? (comp #{:kong} first))
(def chow? (comp #{:chow} first))
(def thirteen-orphans? (comp #{:thirteen-orphans} first))
(def knitted? (comp #{:knitted} first))

(defn all-thirteen-orphans
  [game]
  (filter thirteen-orphans? (:hand game)))

(defn all-knitted
  [game]
  (filter knitted? (:hand game)))

(defn all-pungs
  [game]
  (sort/sort-fans
    (into (filter pung? (:hand game))
          (filter pung? (:fans game)))))

(defn all-concealed-pungs
  [game]
  (sort/sort-fans
    (concat
      (:concealed-kongs game)
      (filter pung? (:hand game)))))

(defn all-kongs
  [game]
  (sort/sort-fans
    (into (filter kong? (:fans game))
          (:concealed-kongs game))))

(defn all-pairs
  [game]
  (sort/sort-fans
    (filter pair? (:hand game))))

(defn all-chows
  [game]
  (sort/sort-fans
    (into (filter chow? (:hand game))
          (filter chow? (:fans game)))))

(defn fan->tiles
  [[fan-type & [t1 :as tiles]]]
  (case fan-type
    :pung [t1 t1 t1]
    :chow tiles
    :pair [t1 t1]
    :kong [t1 t1 t1]                                        ;; count for 3, because we would get more than 14 tiles otherwise
    :thirteen-orphans tiles
    :knitted tiles))

(defn all-tiles
  [& fans]
  {:post [(= 14 (count %))]}
  (sort (mapcat fan->tiles (reduce into fans))))

(defn all-fans
  [& fans]
  (sort/sort-fans (reduce into fans)))

(defn remove-exclusions
  [fans]
  (reduce
    (fn [acc-fans {:keys [key] :as fan}]
      (if ((set (mapcat :exclusions fans)) key)
        acc-fans
        (conj acc-fans fan)))
    []
    fans))

(defn scoring
  [game]
  (doall (for [hand (find-valid-patterns (:hand game))
               :let [game             (assoc game :hand hand)
                     pungs            (all-pungs game)
                     concealed-pungs  (all-concealed-pungs game)
                     kongs            (all-kongs game)
                     thirteen-orphans (all-thirteen-orphans game)
                     knitted          (all-knitted game)
                     pungs-or-kongs   (sort/sort-fans (into pungs kongs))
                     pairs            (all-pairs game)
                     chows            (all-chows game)
                     all-tiles        (all-tiles thirteen-orphans
                                                 pungs-or-kongs
                                                 pairs
                                                 chows
                                                 knitted)
                     all-fans         (all-fans thirteen-orphans
                                                pungs-or-kongs
                                                pairs
                                                chows)
                     context          (sc.api/spy {:game            game
                                                   :hand            hand
                                                   :pungs           pungs
                                                   :kongs           kongs
                                                   :pairs           pairs
                                                   :pungs-or-kongs  pungs-or-kongs
                                                   :knitted         knitted
                                                   :chows           chows
                                                   :distinct-chows  (set chows)
                                                   :all-tiles       all-tiles
                                                   :all-fans        all-fans
                                                   :concealed-pungs concealed-pungs})]]
           (doall (->> (for [fan fans/fans
                             :let [[_k {:keys [predicate] :as r}] fan]
                             :when (predicate context)] r)
                       (remove-exclusions))))))