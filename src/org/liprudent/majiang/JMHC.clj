(import
  '(org.liprudent.majiang HuFinderWrapper$))

(defn- call-jmhc [json]
  (let [Wrapper (org.liprudent.majiang.HuFinderWrapper$/MODULE$)]
    (. Wrapper (wrapOrError ""))))

(defn- to-json [game player]
  "{}")

(defn hule-details [game player]
  (let [json (to-json game player)]
    (call-jmhc json)))
