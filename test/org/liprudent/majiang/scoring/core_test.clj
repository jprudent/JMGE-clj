(ns org.liprudent.majiang.scoring.core-test
  (:require [clojure.test :refer :all])
  (:require [org.liprudent.majiang.scoring.core :as sut]
            [org.liprudent.majiang.scoring.fans :as fans]))

(deftest valid-pattern?-test
  (is (sut/valid-pattern? [:b5 :b5]))
  (is (sut/valid-pattern? [:b6 :b5 :b5 :b5 :b6]))
  (is (sut/valid-pattern? [:b1 :b1 :b1 :b5 :b5]))
  (is (sut/valid-pattern? [:b1 :b1 :b1 :b2 :b3]))
  (is (sut/valid-pattern? [:b1 :b1 :b1 :b2 :b3 :b2 :b2 :b2]))
  (is (sut/valid-pattern? [:b1 :b2 :b3 :b1 :b2 :b3 :b1 :b2 :b3 :b4 :b4])))

(deftest find-valid-pattern-test
  (is (= #{[[:pung :b9]
            [:chow :b1 :b2 :b3]
            [:chow :b4 :b5 :b6]
            [:chow :b6 :b7 :b8]
            [:pair :b1]]}
         (sut/find-valid-patterns [:b1 :b1 :b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9 :b9 :b9 :b6])))
  (is (= #{[[:pung :s1]
            [:chow :b1 :b2 :b3]
            [:chow :b1 :b2 :b3]
            [:chow :b1 :b2 :b3]
            [:pair :s2]]
           [[:pung :b1]
            [:pung :b2]
            [:pung :b3]
            [:pung :s1]
            [:pair :s2]]}
         (sut/find-valid-patterns [:b1 :b1 :b1
                                   :b2 :b2 :b2
                                   :b3 :b3 :b3
                                   :s1 :s1 :s1
                                   :s2 :s2])))
  (is (= #{[[:pair :b1]
            [:pair :c2]
            [:pair :s3]
            [:pair :ww]
            [:pair :we]
            [:pair :wn]
            [:pair :dr]]}
         (sut/find-valid-patterns (shuffle [:b1 :b1
                                            :c2 :c2
                                            :s3 :s3
                                            :ww :ww
                                            :we :we
                                            :wn :wn
                                            :dr :dr]))))

  (is (= #{[[:knitted
             :b2 :b5 :c1 :c7 :dg :dr :dw :s3 :s6 :s9 :we :wn :ws :ww]]}
         (sut/find-valid-patterns (shuffle [:dr :dg :dw :we :ws :ww :wn
                                            :c1 :c7 :b2 :b5 :s3 :s6 :s9])))))

(deftest scoring-test

  (is (= [[(:big-four-winds fans/fans)]]
         (sut/scoring
           {:hand           [:ws :ws :ws
                             :ww :ww :ww
                             :wn :wn :wn
                             :b3 :b3]
            :fans           [[:pung :we]]
            :out            [:ws :discarded]
            :wind           :we
            :prevalent-wind :we
            :seat-wind      :ws})))

  (is (= [[(:big-three-dragons fans/fans)]]
         (sut/scoring
           {:hand           [:dr :dw :dg
                             :dr :dw :dg
                             :dr :dw :dg
                             :s3 :s3]
            :fans           [[:pung :we]]
            :out            [:s3 :discarded]
            :wind           :we
            :prevalent-wind :we
            :seat-wind      :ws})))

  (is (= [[(:pure-shifted-pungs fans/fans)
           (:all-green fans/fans)]
          [(:pure-triple-chow fans/fans)
           (:all-green fans/fans)]]
         (sut/scoring
           {:hand           [:b2 :b3 :b4
                             :b2 :b3 :b4
                             :b2 :b3 :b4
                             :dg :dg]
            :fans           [[:pung :b8]]
            :out            [:dg :discarded]
            :wind           :we
            :prevalent-wind :we
            :seat-wind      :ws})))

  (is (= [[(:nine-gates fans/fans)]]
         (sut/scoring
           {:hand           [:b1 :b1 :b1
                             :b2 :b3 :b4 :b5 :b6 :b7 :b8
                             :b9 :b9 :b9
                             :b6]
            :fans           []
            :out            [:b6 :discarded]
            :wind           :we
            :prevalent-wind :we
            :seat-wind      :ws})))

  (is (= [[(:four-kongs fans/fans)]]
         (sut/scoring
           {:hand            [:b1 :b1]
            :fans            [[:kong :b7] [:kong :b8]]
            :concealed-kongs [[:kong :s7] [:kong :s8]]
            :out             [:b7 :discarded]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:full-flush fans/fans)]
          [(:full-flush fans/fans)]
          [(:seven-shifted-pairs fans/fans)]
          [(:full-flush fans/fans)]]
         (sut/scoring
           {:hand            [:b1 :b1 :b2 :b2 :b3 :b3
                              :b4 :b4 :b5 :b5 :b6 :b6
                              :b7 :b7]
            :fans            []
            :concealed-kongs []
            :out             [:b6 :discarded]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:thirteen-orphans fans/fans)]]
         (sut/scoring
           {:hand            [:b1 :b9 :c1 :c9 :s1 :s9
                              :dg :dr :dw
                              :we :wn :ws :ww
                              :b1]
            :fans            []
            :concealed-kongs []
            :out             [:b9 :discarded]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:all-terminals fans/fans)]]
         (sut/scoring
           {:hand            [:s1 :s1 :s1
                              :c9 :c9]
            :fans            [[:pung :b1] [:pung :b9]]
            :concealed-kongs [[:kong :s9]]
            :out             [:b9 :discarded]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:little-four-winds fans/fans)]]
         (sut/scoring
           {:hand            [:we :we :b1 :b2 :b3]
            :fans            [[:pung :ws] [:pung :ww]]
            :concealed-kongs [[:kong :wn]]
            :out             [:b9 :discarded]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:little-three-dragons fans/fans)]]
         (sut/scoring
           {:hand            [:dw :dw :b1 :b2 :b3]
            :fans            [[:pung :dr] [:pung :dg]]
            :concealed-kongs [[:kong :b1]]
            :out             [:dw :discarded]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:all-honors fans/fans)]]
         (sut/scoring
           {:hand            [:dw :dw :we :we :we]
            :fans            [[:pung :ww] [:pung :dg]]
            :concealed-kongs [[:kong :ws]]
            :out             [:dw :discarded]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:four-concealed-pungs fans/fans)]]
         (sut/scoring
           {:hand            [:dw :dw
                              :we :we :we
                              :b2 :b2 :b2
                              :s5 :s5 :s5
                              :dg :dg :dg]
            :fans            []
            :concealed-kongs []
            :out             [:dw :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:pure-terminal-chows fans/fans)]]
         (sut/scoring
           {:hand            [:b5 :b5
                              :b1 :b2 :b3
                              :b7 :b8 :b9]
            :fans            [[:chow :b1 :b2 :b3]
                              [:chow :b7 :b8 :b9]]
            :concealed-kongs []
            :out             [:dw :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:quadruple-chow fans/fans)]]
         (sut/scoring
           {:hand            [:s5 :s5
                              :b1 :b2 :b3]
            :fans            [[:chow :b1 :b2 :b3]
                              [:chow :b1 :b2 :b3]
                              [:chow :b1 :b2 :b3]]
            :concealed-kongs []
            :out             [:s5 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:four-pure-shifted-pungs fans/fans)]]
         (sut/scoring
           {:hand            [:s5 :s5
                              :c5 :c5 :c5]
            :fans            [[:pung :c6]
                              [:kong :c7]]
            :concealed-kongs [[:kong :c8]]
            :out             [:s5 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:four-shifted-chows fans/fans)]]
         (sut/scoring
           {:hand            [:s5 :s5
                              :c1 :c2 :c3]
            :fans            [[:chow :c3 :c4 :c5]           ;; a shift by 2
                              [:chow :c5 :c6 :c7]
                              [:chow :c7 :c8 :c9]]
            :concealed-kongs []
            :out             [:s5 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:three-kongs fans/fans)]]
         (sut/scoring
           {:hand            [:s5 :s5
                              :c1 :c2 :c3]
            :fans            [[:kong :c4]
                              [:kong :dr]]
            :concealed-kongs [[:kong :b1]]
            :out             [:s5 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:all-terminals-and-honors fans/fans)]]
         (sut/scoring
           {:hand            [:s1 :s1
                              :dr :dr :dr]
            :fans            [[:kong :ww]
                              [:pung :c9]]
            :concealed-kongs [[:kong :b1]]
            :out             [:s5 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:seven-pairs fans/fans)]]
         (sut/scoring
           {:hand            [:b2 :b2
                              :b3 :b3
                              :b7 :b7
                              :dr :dr
                              :c2 :c2
                              :c1 :c1
                              :c8 :c8]
            :fans            []
            :concealed-kongs []
            :out             [:s5 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:greater-honors-and-knitted-tiles fans/fans)]]
         (sut/scoring
           {:hand            [:dr :dg :dw :we :ws :ww :wn
                              :c1 :c7 :b2 :b5 :s3 :s6 :s9]
            :fans            []
            :concealed-kongs []
            :out             [:c1 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:all-even-pungs fans/fans)]]
         (sut/scoring
           {:hand            [:s2 :s2
                              :b4 :b4 :b4]
            :fans            [[:pung :b6] [:kong :c2]]
            :concealed-kongs [[:kong :s8]]
            :out             [:s2 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:full-flush fans/fans)]]
         (sut/scoring
           {:hand            [:b2 :b2
                              :b4 :b4 :b4]
            :fans            [[:pung :b6] [:chow :b6 :b7 :b8]]
            :concealed-kongs [[:kong :b8]]
            :out             [:s2 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:pure-triple-chow fans/fans)]]
         (sut/scoring
           {:hand            [:dr :dr
                              :s1 :s2 :s3]
            :fans            [[:chow :b1 :b2 :b3]
                              [:chow :b1 :b2 :b3]
                              [:chow :b1 :b2 :b3]]
            :concealed-kongs []
            :out             [:s5 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:pure-shifted-pungs fans/fans)]]
         (sut/scoring
           {:hand            [:dr :dr
                              :s1 :s2 :s3]
            :fans            [[:pung :b4]
                              [:kong :b5]]
            :concealed-kongs [[:kong :b3]]
            :out             [:s1 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:upper-tiles fans/fans)]]
         (sut/scoring
           {:hand            [:s7 :s8 :s9 :c9 :c9]
            :fans            [[:pung :b7]
                              [:chow :b7 :b8 :b9]]
            :concealed-kongs [[:kong :c8]]
            :out             [:s8 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:middle-tiles fans/fans)]]
         (sut/scoring
           {:hand            [:s4 :s5 :s6 :c6 :c6]
            :fans            [[:pung :b4]
                              [:chow :b4 :b5 :b6]]
            :concealed-kongs [[:kong :c5]]
            :out             [:s8 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:lower-tiles fans/fans)]]
         (sut/scoring
           {:hand            [:s1 :s2 :s3 :c3 :c3]
            :fans            [[:pung :b2]
                              [:chow :b1 :b2 :b3]]
            :concealed-kongs [[:kong :c2]]
            :out             [:s8 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:full-flush fans/fans)
           (:pure-straight fans/fans)]]
         (sut/scoring
           {:hand            [:c4 :c5 :c6
                              :c7 :c8 :c9
                              :c1 :c2 :c3
                              :c7 :c7]
            :fans            [[:chow :c1 :c2 :c3]]
            :concealed-kongs []
            :out             [:c8 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:three-suited-terminal-chows fans/fans)]]
         (sut/scoring
           {:hand            [:s1 :s2 :s3
                              :s7 :s8 :s9
                              :b5 :b5]
            :fans            [[:chow :c1 :c2 :c3]
                              [:chow :c7 :c8 :c9]]
            :concealed-kongs []
            :out             [:b5 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:pure-shifted-chows fans/fans)]]
         (sut/scoring
           {:hand            [:s5 :s5
                              :c1 :c2 :c3]
            :fans            [[:chow :c3 :c4 :c5]           ;; a shift by 2
                              [:chow :c5 :c6 :c7]
                              [:chow :s7 :s8 :s9]]
            :concealed-kongs []
            :out             [:s5 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:pure-shifted-chows fans/fans)]]
         (sut/scoring
           {:hand            [:s5 :s5
                              :c1 :c2 :c3]
            :fans            [[:chow :c3 :c4 :c5]           ;; a shift by 2
                              [:chow :c5 :c6 :c7]
                              [:chow :c2 :c3 :c4]]
            :concealed-kongs []
            :out             [:s5 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws})))

  (is (= [[(:pure-shifted-chows fans/fans)]]
         (sut/scoring
           {:hand            [:s5 :s5
                              :c2 :c3 :c4]
            :fans            [[:chow :c3 :c4 :c5]           ;; a shift by 1
                              [:chow :c4 :c5 :c6]
                              [:chow :c2 :c3 :c4]]
            :concealed-kongs []
            :out             [:s5 :self-drawn]
            :wind            :we
            :prevalent-wind  :we
            :seat-wind       :ws}))))
