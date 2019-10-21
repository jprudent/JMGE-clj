(ns org.liprudent.majiang.scoring.sort-test
  (:require [clojure.test :refer :all])
  (:require [org.liprudent.majiang.scoring.sort :as sut]))

(deftest sort-fans-test
  (is (= [[:pung :b1] [:kong :b2] [:pung :b3]]
         (sut/sort-fans [[:kong :b2] [:pung :b3] [:pung :b1]])))

  (is (= [[:pung :c5] [:pung :c6] [:kong :c7] [:kong :c8]]
         (sut/sort-fans [[:kong :c7] [:kong :c8] [:pung :c6] [:pung :c5]])))

  (is (= [[:pung :c5] [:kong :c6] [:pung :c7] [:kong :c8]]
         (sut/sort-fans [[:pung :c7] [:kong :c8] [:kong :c6] [:pung :c5]])))

  (is (= [[:pair :b1] [:pair :c2] [:pair :s3] [:pair :ww] [:pair :we] [:pair :wn] [:pair :dr]]
         (sut/sort-fans
           (shuffle [[:pair :b1] [:pair :c2] [:pair :s3] [:pair :ww] [:pair :we] [:pair :wn] [:pair :dr]])))))

