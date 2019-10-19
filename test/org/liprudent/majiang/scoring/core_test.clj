(ns org.liprudent.majiang.scoring.core-test
  (:require [clojure.test :refer :all])
  (:require [org.liprudent.majiang.scoring.core :refer [valid-pattern?]]))

(deftest valid-hule?-test
  (is (valid-pattern? [:b5 :b5]))
  (is (valid-pattern? [:b6 :b5 :b5 :b5 :b6]))
  (is (valid-pattern? [:b1 :b1 :b1 :b5 :b5]))
  (is (valid-pattern? [:b1 :b1 :b1 :b2 :b3]))
  (is (valid-pattern? [:b1 :b1 :b1 :b2 :b3 :b2 :b2 :b2]))
  (is (valid-pattern? [:b1 :b2 :b3 :b1 :b2 :b3 :b1 :b2 :b3 :b4 :b4])))
