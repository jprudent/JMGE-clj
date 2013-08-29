(ns majiang-test
 (:use clojure.test)
 (:use majiang))

(def aggregate-id 11)

(clear-events in-memory-event-store aggregate-id)

(deftest four-people-joining
  (let [cmd-enter (->NewPlayerEnter aggregate-id)]

    (is (= (->Game nil 0) (replay-all aggregate-id)))

    (handle-command cmd-enter in-memory-event-store)
    (is (= (->Game nil 1) (replay-all aggregate-id)))

    (handle-command cmd-enter in-memory-event-store)
    (is (= (->Game nil 2) (replay-all aggregate-id)))

    (handle-command cmd-enter in-memory-event-store)
    (is (= (->Game nil 3) (replay-all aggregate-id)))

    (handle-command cmd-enter in-memory-event-store)
    (is (= (->Game nil 4) (replay-all aggregate-id)))

    (is (thrown? Exception (handle-command cmd-enter in-memory-event-store)))))

(with-test-out (run-tests))

(println (+ 1 (rand-int 6)))


