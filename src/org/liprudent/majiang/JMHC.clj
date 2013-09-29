(import
  '(org.liprudent.majiang HuFinderWrapper$))

(defn- call-jmhc [json]
  (let [Wrapper (org.liprudent.majiang.HuFinderWrapper$/MODULE$)]
    (. Wrapper (wrapOrError ""))))

(defn- q [s] (str "\"" s "\""))

(defn- symbol-to-string [sym] (subs (str sym) 1))

(defn- symbols-to-string [symbols] (map symbol-to-string symbols))

(defn- to-json-concealed [tiles]
  (str
    (q "concealed") ":"
    (q (clojure.string/join " " (symbols-to-string tiles)))))

(defn- fan-to-string [fan]
  (let [kind (get fan 0)
        tiles (symbols-to-string (subvec fan 1))]
    (condp = kind
      :chow (clojure.string/join "-" tiles)
      :pung (clojure.string/join "-" (concat tiles tiles tiles))
      :kong (clojure.string/join "-" (concat tiles tiles tiles tiles)))))

(defn- to-json-melded [fans]
  (str
    (q "melded") ":"
    (q (clojure.string/join " "  (map fan-to-string fans)))))

(defn to-json [game player]
  (str
    "{"
    (to-json-concealed (get-player-tiles game player)) ","
    (to-json-melded (get-player-fans game player)) ","
    (q "winning_tile") ":" (q (symbol-to-string (get-last-discarded game))) ;; FIXME in case of self drawn
    "}"))

(defn hule-details [game player]
  (let [json (to-json game player)]
    (call-jmhc json)))
