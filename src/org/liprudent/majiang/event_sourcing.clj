(ns org.liprudent.majiang.event-sourcing)

(defmulti apply-event
  ;"A multimethod for handling events. Each event has his own method defined"
  (fn [game event] (class event)))

(defn apply-events
  "Utility method to apply all events to game"
  [game events]
  (reduce apply-event game events))

(defprotocol EventStore
  "A protocol that specifies event store function"
  (retrieve-event-stream [this aggregate-id])
  (append-events [this aggregate-id previous-event-stream events])
  (clear-events [this aggregate-id]))

(defrecord EventStream [version transactions])

(import java.util.concurrent.ConcurrentHashMap)
(import java.util.ConcurrentModificationException)

(defn in-memory-event-store
  []
  (let [streams (ConcurrentHashMap.)
        empty-stream (->EventStream 0 [])]
    (reify EventStore
      (retrieve-event-stream [this aggregate-id]
        (if (.putIfAbsent streams aggregate-id empty-stream)
          (.get streams aggregate-id)
          empty-stream))

      (append-events [this aggregate-id previous-es events]
        (let [next-es (->EventStream (inc (:version previous-es)) (conj (:transactions previous-es) events))
              replaced (.replace streams aggregate-id previous-es next-es)]
          (when-not replaced (throw (ConcurrentModificationException.)))))

      (clear-events [this aggregate-id]
        (.put streams aggregate-id empty-stream)))))


