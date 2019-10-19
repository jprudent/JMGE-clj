(ns org.liprudent.majiang.utils)

(defn exception

  ([msg]
  (throw (ex-info msg {})))

  ([msg1 & more]
  (exception (apply str msg1 more))))

(defn minus [v1 v2]
  "substract 2 vectors, v2 must be a subset of v1"
  (reduce (fn [acc v]
            (let [index (.indexOf acc v)]
              (into (subvec acc 0 index) (subvec acc (inc index)))))
          v1 v2))

(defn move-tile
  "move a tile from source to destination"
  [{[t & ts] :source destination :destination :as tile-move}]
  {:pre [(not (nil? t))]}

  (assoc tile-move :source ts :destination (conj destination t)))

(defn move-tiles
  [nb-tiles source destination]
  {:pre [(>= (count source) nb-tiles)]}

  (let [taken (subvec source 0 nb-tiles)
        new-source (subvec source nb-tiles)
        new-destination (into taken destination)]
    [new-source new-destination]))

