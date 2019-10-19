(ns org.liprudent.majiang.scoring.core
  (:require [org.liprudent.majiang.model :as m]
            [clojure.math.combinatorics :as combo]))

(def all-suits
  (set (for [suit-number (partition 3 1 (range 1 10))
             family      [\b \c \s]]
         (map #(keyword (str family %)) suit-number))))

(defn pung? [[t1 t2 t3 & others]]
  (and (nil? others) (= t1 t2 t3)))

(defn pair? [[t1 t2 & others]]
  (and (nil? others) (= t1 t2)))

(defn chow? [tiles]
  (and (= 3 (count tiles)) (all-suits tiles)))

(defn ceil [x] (int (Math/ceil x)))

(defn find-valid-patterns
  "given a set of tiles, return all possible sets of patterns"
  [tiles]
  (let [nb-fans (ceil (/ (count tiles) 3))]
    (filter
      #(every? (some-fn pair? pung? chow?) %)
      (combo/partitions (sort tiles) :min nb-fans :max nb-fans))))

(defn valid-pattern?
  "returns true if tiles can be arranged as a set of valid patterns"
  [tiles]
  (some? (find-valid-patterns tiles)))