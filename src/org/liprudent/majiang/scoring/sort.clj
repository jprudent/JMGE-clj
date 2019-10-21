(ns org.liprudent.majiang.scoring.sort
  (:require [org.liprudent.majiang.model :as m]))

(def fan-order {:pair 33 :chow 22 :pung 11 :kong 11})
(def family-order (zipmap [\b \c \s \w \d] (range 900 1000 1)))
(def f-ranking
  (merge (zipmap (map #(char (+ 0x30 %)) (range 10 0 -1))
                 (range 10 0 -1))
         (zipmap [\e \s \w \n] (range 90 100 1))
         (zipmap [\r \g \w] (range 40 50 1))))              

(defn family-ranking
  [tile]
  (f-ranking (last (name tile))))

(defn sort-fans
  [fans]
  (sort-by (juxt (comp fan-order first)
                 (comp family-order m/family second)
                 (comp family-ranking second))
           fans))

(defn sort-tiles [tiles]
  (sort tiles))