(ns org.liprudent.majiang.scoring.core
  (:require [org.liprudent.majiang.model :as m]
            [clojure.math.combinatorics :as combo]))

(def all-suits
  (set (for [suit-number (partition 3 1 (range 1 10))
             family      [\b \c \s]]
         (map #(keyword (str family %)) suit-number))))

(defn pung? [[t1 t2 t3 & others]]
  (and (nil? others) (= t1 t2 t3)))

(defn chow? [tiles]
  (and (= 3 (count tiles)) (all-suits tiles)))

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
  (let [[[tile nb] :as remaining] (filter (fn [[k v]] (pos-int? v)) freqs)]
    (when (and (= 1 (count remaining)) (= 2 nb))
      {tile nb})))

(defn fan-combinations
  [nb-fan-target
   hand-freqs
   [fan-freq & other-fan-freqs :as fan-freqs]]
  (if (and fan-freq (pos-int? nb-fan-target)
           ;; it will never make it otherwise:
           (>= (count fan-freqs) nb-fan-target))
    (mapcat
      identity
      ;; either try to use fan-freq
      [(let [new-hand-freq (merge-with - hand-freqs fan-freq)
             new-fan-freqs (if (= 3 (count fan-freq))
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

(def fan-order {:pair 33 :chow 22 :pung 11})

(defn- sort-fans
  [fans]
  (sort-by (juxt (comp fan-order first) second) fans))

(defn find-valid-patterns
  [tiles]
  (let [fan-freqs     (->> (combo/combinations tiles 3)
                           (filter (some-fn pung? chow?))
                           (map frequencies))
        tile-freqs    (frequencies tiles)
        nb-fan-target (int (/ (count tiles) 3))]
    (->> (fan-combinations nb-fan-target tile-freqs fan-freqs)
         (map fan-freqs->fan)
         (map sort-fans)
         (set))))

(defn valid-pattern?
  "returns true if tiles can be arranged as a set of valid patterns"
  [tiles]
  (some? (find-valid-patterns tiles)))
