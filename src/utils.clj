(ns majiang)

(defn exception

  ([msg]
  (throw (Exception. msg)))

  ([msg1 & more]
  (exception (apply str msg1 more))))

(defn minus [v1 v2]
  "substract 2 vectors, v2 must be a subset of v1"
  (reduce (fn [acc v]
            (let [index (.indexOf acc v)]
              (into (subvec acc 0 index) (subvec acc (inc index))))) v1 v2))

(defn- move-tile
  "move a tile from source to destination"
  [{[t & ts] :source destination :destination :as tile-move}]
  {:pre [(not (nil? t))]}

  (assoc tile-move :source ts :destination (conj destination t)))
