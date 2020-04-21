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


(defn score
  [game]
  (map #(map (juxt :key :matching-fans) %) (sut/scoring game)))

(deftest score-big-four-winds-test
  (is (= [[#_(:three-concealed-pungs fans/fans)
           [:big-four-winds #{[[:pung :ww]
                               [:pung :we]
                               [:pung :ws]
                               [:pung :wn]]}]]]
         (score
          {:hand [:ws :ws :ws
                  :ww :ww :ww
                  :wn :wn :wn
                  :b3 :b3]
           :fans [[:pung :we]]
           :out [:ws :discarded]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-big-three-dragons-test
  (is (= [[[:big-three-dragons #{[[:pung :dr]
                                  [:pung :dg]
                                  [:pung :dw]]}]]]
         (score
          {:hand [:dw :dg
                  :dw :dg
                  :dw :dg
                  :s3 :s3]
           :fans [[:pung :we]
                  [:pung :dr]]
           :out [:s3 :discarded]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-all-green-test
  (is (= [[[:pure-shifted-pungs #{[[:pung :b2]
                                   [:pung :b3]
                                   [:pung :b4]]}]
           [:all-green #{[[:pung :b2]
                          [:pung :b3]
                          [:pung :b4]
                          [:pung :b8]
                          [:pair :dg]]}]]
          [[:pure-triple-chow #{[[:chow :b2 :b3 :b4]
                                 [:chow :b2 :b3 :b4]
                                 [:chow :b2 :b3 :b4]]}]
           [:all-green #{[[:pung :b8]
                          [:chow :b2 :b3 :b4]
                          [:chow :b2 :b3 :b4]
                          [:chow :b2 :b3 :b4]
                          [:pair :dg]]}]]]
         (score
          {:hand [:b2 :b3 :b4
                  :b2 :b3 :b4
                  :b2 :b3 :b4
                  :dg :dg]
           :fans [[:pung :b8]]
           :out [:dg :discarded]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-nine-gates-test
  (is (= [[[:nine-gates #{[[:pung :b9]
                           [:chow :b1 :b2 :b3]
                           [:chow :b4 :b5 :b6]
                           [:chow :b6 :b7 :b8]
                           [:pair :b1]]}]]]
         (score
          {:hand [:b1 :b1 :b1
                  :b2 :b3 :b4 :b5 :b6 :b7 :b8
                  :b9 :b9 :b9
                  :b6]
           :fans []
           :out [:b6 :discarded]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-four-kongs-test
  (is (= [[[:four-kongs #{[[:kong :b7]
                           [:kong :b8]
                           [:kong :s7]
                           [:kong :s8]]}]]]
         (score
          {:hand [:b1 :b1]
           :fans [[:kong :b7] [:kong :b8]]
           :concealed-kongs [[:kong :s7] [:kong :s8]]
           :out [:b7 :discarded]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-seven-shifted-pairs-test
  (is (= [[[:full-flush #{[[:chow :b2 :b3 :b4]
                           [:chow :b2 :b3 :b4]
                           [:chow :b5 :b6 :b7]
                           [:chow :b5 :b6 :b7]
                           [:pair :b1]]}]]
          [[:full-flush #{[[:chow :b1 :b2 :b3]
                           [:chow :b1 :b2 :b3]
                           [:chow :b5 :b6 :b7]
                           [:chow :b5 :b6 :b7]
                           [:pair :b4]]}]]
          [[:seven-shifted-pairs #{[[:pair :b1]
                                    [:pair :b2]
                                    [:pair :b3]
                                    [:pair :b4]
                                    [:pair :b5]
                                    [:pair :b6]
                                    [:pair :b7]]}]]
          [[:full-flush #{[[:chow :b1 :b2 :b3]
                           [:chow :b1 :b2 :b3]
                           [:chow :b4 :b5 :b6]
                           [:chow :b4 :b5 :b6]
                           [:pair :b7]]}]]]
         (score
          {:hand [:b1 :b1 :b2 :b2 :b3 :b3
                  :b4 :b4 :b5 :b5 :b6 :b6
                  :b7 :b7]
           :fans []
           :concealed-kongs []
           :out [:b6 :discarded]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-thirteen-orphans-test
  (is (= [[[:thirteen-orphans #{[[:thirteen-orphans
                                  :b1 :b9 :c1 :c9 :s1 :s9 :dg
                                  :dr :dw :we :wn :ws :ww :b1]]}]]]
         (score
          {:hand [:b1 :b9 :c1 :c9 :s1 :s9
                  :dg :dr :dw
                  :we :wn :ws :ww
                  :b1]
           :fans []
           :concealed-kongs []
           :out [:b9 :discarded]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-all-terminals-test
  (is (= [[[:all-terminals #{[[:pung :b1]
                              [:pung :b9]
                              [:pung :s1]
                              [:kong :s9]
                              [:pair :c9]]}]]]
         (score
          {:hand [:s1 :s1 :s1 :c9 :c9]
           :fans [[:pung :b1] [:pung :b9]]
           :concealed-kongs [[:kong :s9]]
           :out [:b9 :discarded]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-little-four-winds-test
  (is (= [[[:little-four-winds #{[[:pung :ww]
                                  [:pung :ws]
                                  [:kong :wn]
                                  [:pair :we]]}]]]
         (score
          {:hand [:we :we :b1 :b2 :b3]
           :fans [[:pung :ws] [:pung :ww]]
           :concealed-kongs [[:kong :wn]]
           :out [:b9 :discarded]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-little-three-dragons-test
  (is (= [[[:little-three-dragons #{[[:pung :dr]
                                     [:pung :dg]
                                     [:pair :dw]]}]]]
         (score
          {:hand [:dw :dw :b1 :b2 :b3]
           :fans [[:pung :dr] [:pung :dg]]
           :concealed-kongs [[:kong :b1]]
           :out [:dw :discarded]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-all-honors-test
  (is (= [[[:all-honors #{[[:pung :ww]
                           [:pung :we]
                           [:kong :ws]
                           [:pung :dg]
                           [:pair :dw]]}]]]
         (score
          {:hand [:dw :dw :we :we :we]
           :fans [[:pung :ww] [:pung :dg]]
           :concealed-kongs [[:kong :ws]]
           :out [:dw :discarded]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-four-concealed-pungs-test
  (is (= [[[:four-concealed-pungs #{[[:pung :b2]
                                     [:pung :s5]
                                     [:pung :we]
                                     [:pung :dg]]}]]]
         (score
          {:hand [:dw :dw
                  :we :we :we
                  :b2 :b2 :b2
                  :s5 :s5 :s5
                  :dg :dg :dg]
           :fans []
           :concealed-kongs []
           :out [:dw :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-pure-terminal-chows-test
  (is (= [[[:pure-terminal-chows #{[[:chow :b1 :b2 :b3]
                                    [:chow :b1 :b2 :b3]
                                    [:chow :b7 :b8 :b9]
                                    [:chow :b7 :b8 :b9]
                                    [:pair :b5]]}]]]
         (score
          {:hand [:b5 :b5
                  :b1 :b2 :b3
                  :b7 :b8 :b9]
           :fans [[:chow :b1 :b2 :b3]
                  [:chow :b7 :b8 :b9]]
           :concealed-kongs []
           :out [:dw :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-quadruple-chow-test
  (is (= [[[:quadruple-chow #{[[:chow :b1 :b2 :b3]
                               [:chow :b1 :b2 :b3]
                               [:chow :b1 :b2 :b3]
                               [:chow :b1 :b2 :b3]]}]]]
         (score
          {:hand [:s5 :s5
                  :b1 :b2 :b3]
           :fans [[:chow :b1 :b2 :b3]
                  [:chow :b1 :b2 :b3]
                  [:chow :b1 :b2 :b3]]
           :concealed-kongs []
           :out [:s5 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-four-pure-shifted-pungs-test
  (is (= [[[:four-pure-shifted-pungs #{[[:pung :c5]
                                        [:pung :c6]
                                        [:kong :c7]
                                        [:kong :c8]]}]]]
         (score
          {:hand [:s5 :s5
                  :c5 :c5 :c5]
           :fans [[:pung :c6]
                  [:kong :c7]]
           :concealed-kongs [[:kong :c8]]
           :out [:s5 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-four-shifted-chows-test
  (is (= [[[:four-shifted-chows #{[[:chow :c1 :c2 :c3]
                                   [:chow :c3 :c4 :c5]
                                   [:chow :c5 :c6 :c7]
                                   [:chow :c7 :c8 :c9]]}]]]
         (score
          {:hand [:s5 :s5
                  :c1 :c2 :c3]
           :fans [[:chow :c3 :c4 :c5]                                           ;; a shift by 2
                  [:chow :c5 :c6 :c7]
                  [:chow :c7 :c8 :c9]]
           :concealed-kongs []
           :out [:s5 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-three-kongs-test
  (is (= [[[:three-kongs #{[[:kong :b1]
                            [:kong :c4]
                            [:kong :dr]]}]]]
         (score
          {:hand [:s5 :s5
                  :c1 :c2 :c3]
           :fans [[:kong :c4]
                  [:kong :dr]]
           :concealed-kongs [[:kong :b1]]
           :out [:s5 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-all-terminals-and-honors-test
  (is (= [[[:all-terminals-and-honors #{[[:kong :b1]
                                         [:pung :c9]
                                         [:kong :ww]
                                         [:pung :dr]
                                         [:pair :s1]]}]]]
         (score
          {:hand [:s1 :s1
                  :dr :dr :dr]
           :fans [[:kong :ww]
                  [:pung :c9]]
           :concealed-kongs [[:kong :b1]]
           :out [:s5 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-seven-pairs-test
  (is (= [[[:seven-pairs #{[[:pair :b2]
                            [:pair :b3]
                            [:pair :b7]
                            [:pair :c1]
                            [:pair :c2]
                            [:pair :c8]
                            [:pair :dr]]}]]]
         (score
          {:hand [:b2 :b2
                  :b3 :b3
                  :b7 :b7
                  :dr :dr
                  :c2 :c2
                  :c1 :c1
                  :c8 :c8]
           :fans []
           :concealed-kongs []
           :out [:s5 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-greater-honors-and-knitted-tiles-test
  (is (= [[[:greater-honors-and-knitted-tiles #{[[:knitted :b2 :b5 :c1 :c7 :dg :dr :dw :s3 :s6 :s9 :we :wn :ws :ww]]}]]]
         (score
          {:hand [:dr :dg :dw :we :ws :ww :wn
                  :c1 :c7 :b2 :b5 :s3 :s6 :s9]
           :fans []
           :concealed-kongs []
           :out [:c1 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-all-even-pungs-test
  (is (= [[[:all-even-pungs #{[[:pung :b4]
                               [:pung :b6]
                               [:kong :c2]
                               [:kong :s8]]}]]]
         (score
          {:hand [:s2 :s2
                  :b4 :b4 :b4]
           :fans [[:pung :b6] [:kong :c2]]
           :concealed-kongs [[:kong :s8]]
           :out [:s2 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-full-flush-test
  (is (= [[[:full-flush #{[[:pung :b4]
                           [:pung :b6]
                           [:kong :b8]
                           [:chow :b6 :b7 :b8]
                           [:pair :b2]]}]]]
         (score
          {:hand [:b2 :b2
                  :b4 :b4 :b4]
           :fans [[:pung :b6] [:chow :b6 :b7 :b8]]
           :concealed-kongs [[:kong :b8]]
           :out [:s2 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-pure-triple-chow-test
  (is (= [[[:pure-triple-chow #{[[:chow :b1 :b2 :b3]
                                 [:chow :b1 :b2 :b3]
                                 [:chow :b1 :b2 :b3]]}]]]
         (score
          {:hand [:dr :dr
                  :s1 :s2 :s3]
           :fans [[:chow :b1 :b2 :b3]
                  [:chow :b1 :b2 :b3]
                  [:chow :b1 :b2 :b3]]
           :concealed-kongs []
           :out [:s5 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-pure-shifted-pungs-test
  (is (= [[[:pure-shifted-pungs #{[[:kong :b3]
                                   [:pung :b4]
                                   [:kong :b5]]}]]]
         (score
          {:hand [:dr :dr
                  :s1 :s2 :s3]
           :fans [[:pung :b4]
                  [:kong :b5]]
           :concealed-kongs [[:kong :b3]]
           :out [:s1 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-upper-tiles-test
  (is (= [[[:upper-tiles #{[[:pung :b7]
                            [:kong :c8]
                            [:chow :b7 :b8 :b9]
                            [:chow :s7 :s8 :s9]
                            [:pair :c9]]}]]]
         (score
          {:hand [:s7 :s8 :s9 :c9 :c9]
           :fans [[:pung :b7]
                  [:chow :b7 :b8 :b9]]
           :concealed-kongs [[:kong :c8]]
           :out [:s8 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-middle-tiles-test
  (is (= [[[:middle-tiles #{[[:pung :b4]
                             [:kong :c5]
                             [:chow :b4 :b5 :b6]
                             [:chow :s4 :s5 :s6]
                             [:pair :c6]]}]]]
         (score
          {:hand [:s4 :s5 :s6 :c6 :c6]
           :fans [[:pung :b4]
                  [:chow :b4 :b5 :b6]]
           :concealed-kongs [[:kong :c5]]
           :out [:s8 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-lower-tiles-test
  (is (= [[[:lower-tiles #{[[:pung :b2]
                            [:kong :c2]
                            [:chow :b1 :b2 :b3]
                            [:chow :s1 :s2 :s3]
                            [:pair :c3]]}]]]
         (score
          {:hand [:s1 :s2 :s3 :c3 :c3]
           :fans [[:pung :b2]
                  [:chow :b1 :b2 :b3]]
           :concealed-kongs [[:kong :c2]]
           :out [:s8 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-pure-straight-test
  (is (= [[[:pure-straight #{[[:chow :c1 :c2 :c3]
                              [:chow :c4 :c5 :c6]
                              [:chow :c7 :c8 :c9]]}]]]
         (score
          {:hand [:c4 :c5 :c6
                  :c7 :c8 :c9
                  :b1 :b2 :b3
                  :c7 :c7]
           :fans [[:chow :c1 :c2 :c3]]
           :concealed-kongs []
           :out [:c8 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-three-suited-terminal-chows-test
  (is (= [[[:three-suited-terminal-chows #{[[:chow :c1 :c2 :c3]
                                            [:chow :c7 :c8 :c9]
                                            [:chow :s1 :s2 :s3]
                                            [:chow :s7 :s8 :s9]
                                            [:pair :b5]]}]]]
         (score
          {:hand [:s1 :s2 :s3
                  :s7 :s8 :s9
                  :b5 :b5]
           :fans [[:chow :c1 :c2 :c3]
                  [:chow :c7 :c8 :c9]]
           :concealed-kongs []
           :out [:b5 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

(deftest score-pure-shifted-chows-test
  (is (= [[[:pure-shifted-chows #{[[:chow :c1 :c2 :c3]
                                   [:chow :c3 :c4 :c5]
                                   [:chow :c5 :c6 :c7]]}]]]
         (score
          {:hand [:s5 :s5
                  :c1 :c2 :c3]
           :fans [[:chow :c3 :c4 :c5]                                       ;; a shift by 2
                  [:chow :c5 :c6 :c7]
                  [:chow :s7 :s8 :s9]]
           :concealed-kongs []
           :out [:s5 :self-drawn]
           :wind :we
           :prevalent-wind :we
           :seat-wind :ws}))))

#_(deftest scoring-test



    #_(is (= [[(:pure-shifted-chows fans/fans)]]
             (sut/scoring
              {:hand [:s5 :s5
                      :c1 :c2 :c3]
               :fans [[:chow :c3 :c4 :c5]                                       ;; a shift by 2
                      [:chow :c5 :c6 :c7]
                      [:chow :s7 :s8 :s9]]
               :concealed-kongs []
               :out [:s5 :self-drawn]
               :wind :we
               :prevalent-wind :we
               :seat-wind :ws})))

    #_(is (= [[(:pure-shifted-chows fans/fans)]]
             (sut/scoring
              {:hand [:s5 :s5
                      :c1 :c2 :c3]
               :fans [[:chow :c3 :c4 :c5]                                       ;; a shift by 2
                      [:chow :c5 :c6 :c7]
                      [:chow :c2 :c3 :c4]]
               :concealed-kongs []
               :out [:s5 :self-drawn]
               :wind :we
               :prevalent-wind :we
               :seat-wind :ws})))

    #_(is (= [[(:pure-shifted-chows fans/fans)]]
             (sut/scoring
              {:hand [:s5 :s5
                      :c2 :c3 :c4]
               :fans [[:chow :c3 :c4 :c5]                                       ;; a shift by 1
                      [:chow :c4 :c5 :c6]
                      [:chow :c2 :c3 :c4]]
               :concealed-kongs []
               :out [:s5 :self-drawn]
               :wind :we
               :prevalent-wind :we
               :seat-wind :ws})))

    #_(is (= [[(:all-fives fans/fans)]]
             (sut/scoring
              {:hand [:s4 :s5 :s6
                      :c3 :c4 :c5
                      :b5 :b5]
               :fans [[:pung :c5]
                      [:pung :s5]]
               :concealed-kongs []
               :out [:s5 :self-drawn]
               :wind :we
               :prevalent-wind :we
               :seat-wind :ws})))

    #_(is (= [[(:triple-pungs fans/fans)]]
             (sut/scoring
              {:hand [:s3 :s3 :s3 :b4 :b4]
               :fans [[:pung :c3]
                      [:kong :b3]]
               :concealed-kongs [[:kong :c9]]
               :out [:b4 :self-drawn]
               :wind :we
               :prevalent-wind :we
               :seat-wind :ws})))

    #_(is (= [[(:three-concealed-pungs fans/fans)]]
             (sut/scoring
              {:hand [:s3 :s3 :s3
                      :b4 :b4 :b4
                      :s9 :s9 :s9
                      :c1 :c1]
               :fans [[:pung :c3]]
               :concealed-kongs []
               :out [:b4 :self-drawn]
               :wind :we
               :prevalent-wind :we
               :seat-wind :ws})))

    #_(is (= [[(:lesser-honors-and-knitted fans/fans)]]
             (sut/scoring
              {:hand [:b1 :b4 :b7
                      :c2 :c5 :c8
                      :s3 :s9
                      :dr :dw :dg
                      :wn :we :ws]
               :fans []
               :concealed-kongs []
               :out [:b4 :self-drawn]
               :wind :we
               :prevalent-wind :we
               :seat-wind :ws})))

    #_(is (= [[(:lesser-honors-and-knitted fans/fans)]]
             (sut/scoring
              {:hand [:b1 :b4 :b7
                      :c2 :c5 :c8
                      :s6 :s3 :s9
                      :dr :dw :dg
                      :wn :we]
               :fans []
               :concealed-kongs []
               :out [:b4 :self-drawn]
               :wind :we
               :prevalent-wind :we
               :seat-wind :ws})))

    #_(is (= [[(:knitted-straight fans/fans)]]
             (sut/scoring
              {:hand [:b1 :b4 :b7
                      :c2 :c5 :c8
                      :s6 :s3 :s9
                      :wn :wn]
               :fans []
               :concealed-kongs [[:kong :b2]]
               :out [:b4 :self-drawn]
               :wind :we
               :prevalent-wind :we
               :seat-wind :ws})))

    #_(is (= [[(:upper-four fans/fans)]]
             (sut/scoring
              {:hand [:b7 :b8 :b9
                      :s6 :s7 :s8
                      :b7 :b7]
               :fans [[:chow :c7 :c8 :c9]]
               :concealed-kongs [[:kong :s9]]
               :out [:b7 :self-drawn]
               :wind :we
               :prevalent-wind :we
               :seat-wind :ws})))

    #_(is (= [[(:lower-four fans/fans)]]
             (sut/scoring
              {:hand [:b2 :b3 :b4
                      :s1 :s2 :s3
                      :b2 :b2]
               :fans [[:chow :c2 :c3 :c4]]
               :concealed-kongs [[:kong :s4]]
               :out [:b2 :self-drawn]
               :wind :we
               :prevalent-wind :we
               :seat-wind :ws})))

    #_(is (= [[(:big-three-winds fans/fans)]]
             (sut/scoring
              {:hand [:wn :wn :wn
                      :s1 :s2 :s3
                      :s4 :s4]
               :fans [[:kong :we]]
               :concealed-kongs [[:kong :ws]]
               :out [:b2 :self-drawn]
               :wind :we
               :prevalent-wind :we
               :seat-wind :ws})))

    #_(is (= [[(:big-three-winds fans/fans)
               (:three-concealed-pungs fans/fans)
               (:all-terminals-and-honors fans/fans)]]
             (sut/scoring
              {:hand [:wn :wn :wn
                      :s1 :s1 :s1
                      :s4 :s4]
               :fans [[:kong :we]]
               :concealed-kongs [[:kong :ws]]
               :out [:b2 :self-drawn]
               :wind :we
               :prevalent-wind :we
               :seat-wind :ws}))))
