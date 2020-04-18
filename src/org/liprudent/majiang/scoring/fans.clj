(ns org.liprudent.majiang.scoring.fans
  (:require [clojure.set :as set]
            [org.liprudent.majiang.model :as m]
            [org.liprudent.majiang.scoring.sort :as sort]
            [clojure.core.logic :as l]))

(defn only-tiles
  [& allowed-tiles]
  (fn [{:keys [all-tiles]}]
    (= (set all-tiles)
       (set/intersection (set allowed-tiles)
                         (set all-tiles)))))

(defn fully-concealed?
  [{:keys [hand]}]
  (= 5 (count hand)))

(defn families
  [tiles]
  (map m/family tiles))

(defn full-flush?
  [{:keys [all-tiles]}]
  (= 1 (count (set (families all-tiles)))))

(defn full-flush-min-occurence
  [min-occurences]
  (fn [{:keys [all-tiles]}]
    (let [freq (frequencies (map m/order all-tiles))]
      (->> (merge-with - freq min-occurences)
           (vals)
           (every? nat-int?)))))

(defn hand-min-occurences
  [min-occurences]
  (fn [{:keys [all-tiles]}]
    (let [freq (frequencies all-tiles)
          allowed (keys min-occurences)
          freq (merge (zipmap allowed (repeat 0)) freq)]
      (->> (merge-with - (select-keys freq allowed) min-occurences)
           (vals)
           (every? nat-int?)))))

(defn every-tiles
  [& mandatory-tiles]
  (hand-min-occurences (zipmap mandatory-tiles (repeat 1))))


(defn having-pred
  ([pred reducer init]
   (fn [[fan & other-fans :as fans]]
     (case (count fans)
       0 false
       (pred (if init
               (reduce reducer (init fan) other-fans)
               (reduce reducer fans))))))
  ([pred reducer]
   (having-pred pred reducer nil)))

(defn nb=
  [nb]
  (having-pred #(= nb %)
               (fn [sum _fan] (inc sum))
               (constantly 1)))

(defn all-equals
  ([] (having-pred identity
                   (fn [a b] (and (= a b) a))))
  ([n]
   (having-pred #(some (fn [[_ nb]] (>= nb n)) %)
                (fn [freqs fan] (update freqs fan (fnil inc 0)))
                (fn [fan] {fan 1}))))

(defn same-family
  []
  (having-pred identity
               (fn [last-family [_ & tiles]]
                 (and (= #{last-family}
                         (reduce #(conj %1 (m/family %2)) #{} tiles))
                      last-family))
               (comp m/family second)))

(def f-fan (comp m/family second))
(def o-fan (comp m/order second))

(defn same-family=n
  [family n]
  (fn [fans]
    (->> (group-by f-fan fans)
         (filter (fn [[fam fans]] (and (= fam family)
                                       (= n (count fans)))))
         (not-empty)
         (some?))))

(defn shifted-family
  ([shift] (shifted-family 4 shift))
  ([nb-of-fans shift]
   (fn [fans]
     (->> (group-by f-fan fans)
          (some (fn [[_family fans]]
                  (= nb-of-fans
                     (count
                      (let [os (keep o-fan fans)]
                        (reduce
                         (fn [[x :as acc] y]
                           (if (= shift (- y x))
                             (conj acc y)
                             acc))
                         (take 1 os)
                         (rest os)))))))))))

(defn same-order-distinct-families
  [nb-of-distinct-families]
  (fn [fans]
    (->> (group-by o-fan fans)
         (some (fn [[order fans]]
                 (and order
                      (= nb-of-distinct-families
                         (count fans)
                         (count (set (map f-fan fans))))))))))

(defn at-least-one-of
  "at least one of the tiles"
  [& one-of-tiles]
  (let [one-of-tiles (set one-of-tiles)
        keep-tile (fn [tiles] (set (filter one-of-tiles tiles)))]
    (having-pred
     #(some one-of-tiles %)
     (fn [tiles-seen [_ & tiles]]
       (into tiles-seen (keep-tile tiles)))
     keep-tile)))

(defn even
  []
  (let [tile-even? (comp even? m/order)]
    (having-pred
     identity
     (fn [past-result [_ & tiles]]
       (and past-result (every? tile-even? tiles)))
     tile-even?)))

(defn suits
  []
  (let [suit-tile? (complement m/honors)
        suit-fan? (fn [[_ & tiles]] (every? suit-tile? tiles))]
    (having-pred
     identity
     (fn [past-result fan]
       (and past-result (suit-fan? fan)))
     suit-fan?)))


(defn having
  [kind & having-preds]
  (fn [context]
    ((apply every-pred having-preds) (kind context))))

(defn sortedo
  "the fans are properly sorted"
  [fansl]
  (l/pred fansl #(= % (sort/sort-fans %))))

(defn wind? [[_ tile]] (contains? #{\w} (m/family tile)))

(defn windo
  "the fan is a wind"
  [fan]
  (l/pred fan wind?))


(defn dragon? [[_ tile]] (contains? #{\d} (m/family tile)))

(defn dragono
  "the fan is a dragon"
  [fan]
  (l/pred fan dragon?))

(defn big-four-winds
  "4 pungs or kongs of winds"
  [{:keys [pungs-or-kongs] :as hand}]
  (let [fans (l/run* [fansl]
               (l/fresh [pung1 pung2 pung3 pung4]
                 (l/everyg #(l/membero % pungs-or-kongs)
                           [pung1 pung2 pung3 pung4])
                 (l/everyg windo
                           [pung1 pung2 pung3 pung4])
                 (l/distincto [pung1 pung2 pung3 pung4])
                 (l/== fansl [pung1 pung2 pung3 pung4])
                 (sortedo fansl)))]
    (set fans)))

(defn big-three-dragons
  "3 pungs or kongs of dragons"
  [{:keys [pungs-or-kongs] :as hand}]
  (let [fans (l/run* [fansl]
               (l/fresh [pung1 pung2 pung3]
                 (l/everyg #(l/membero % pungs-or-kongs)
                           [pung1 pung2 pung3])
                 (l/everyg dragono
                           [pung1 pung2 pung3])
                 (l/distincto [pung1 pung2 pung3])
                 (l/== fansl [pung1 pung2 pung3])
                 (sortedo fansl)))]
    (set fans)))

(defn green?
  [fan]
  (every? m/green-tiles (filter m/tile? fan)))

(defn all-green
  [{:keys [all-fans] :as _game}]
  (if (every? green? all-fans)
    #{all-fans}
    #{}))

(def fans
  {:big-four-winds
   {:key :big-four-winds
    :name "Big four winds"
    :description "Pungs or Kongs of all four Wind Tiles"
    :points 88
    :predicate big-four-winds
    :exclusions #{:big-three-winds
                  :all-pungs
                  :prevalent-wind
                  :seat-wind
                  :pung-of-terminals-or-honors

                  ;; exclusion I added
                  :little-four-winds}}
   :big-three-dragons
   {:key :big-three-dragons
    :name "Big three dragons"
    :description "Pungs or Kongs of all three Dragon tiles"
    :points 88
    :predicate big-three-dragons #_(having :pungs-or-kongs (same-family=n \d 3))
    :exclusions #{:two-dragons
                  :dragon-pung

                  ;; exclusion I added
                  :little-three-dragons}}

   :all-green
   {:key :all-green
    :name "All green"
    :description "Hand is composed entirely of any of the 2, 3, 4, 6, 8 of Bamboo and Green Dragon"
    :points 88
    :predicate all-green}

   #_#_:nine-gates
       {:key :nine-gates
        :name "Nine gates"
        :description "Holding the 1, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 9 tiles in one suit, creating the nine-sided wait of 1, 2, 3, 4, 5, 6, 7, 8, 9."
        :points 88
        :predicate (every-pred
                    fully-concealed?
                    full-flush?
                    (full-flush-min-occurence {1 3, 2 1, 3 1, 4 1, 5 1, 6 1, 7 1, 8 1, 9 3}))
        :exclusions #{:full-flush
                      :concealed-hand
                      :pung-of-terminals-or-honors}}

   #_#_:four-kongs
       {:key :four-kongs
        :name "Four kongs"
        :description "A hand that includes four Kongs."
        :points 88
        :predicate (having :kongs (nb= 4))
        :exclusions #{:single-wait}}

   #_#_:seven-shifted-pairs
       {:key :seven-shifted-pairs
        :name "Seven shifted pairs"
        :description "Hand is composed of seven pairs in the same suit, each shifted one up from the last."
        :points 88
        :predicate (having :pairs (nb= 7) (same-family) (shifted-family 7 1))
        :exclusions #{:full-flush
                      :concealed-hand
                      :single-wait

                      ;; exclusion I added
                      :pure-terminal-chows
                      :seven-pairs}}

   #_#_:thirteen-orphans
       {:key :thirteen-orphans
        :name "Thirteen orphans"
        :description "Hand that consists of one of each terminal and honor tile"
        :points 88
        :predicate (every-pred
                    (every-tiles :b1 :b9 :c1 :c9 :dg :dr :dw :s1 :s9 :we :wn :ws :ww)
                    (only-tiles :b1 :b9 :c1 :c9 :dg :dr :dw :s1 :s9 :we :wn :ws :ww))
        :exclusions #{:all-types :concealed-hand :single-wait}}

   #_#_:all-terminals
       {:key :all-terminals
        :name "All terminals"
        :description "The pairs, pungs or kongs are all made up of 1 or 9 number tiles, without Honor Tiles."
        :points 64
        :predicate (only-tiles :b1 :b9 :c1 :c9 :s1 :s9)
        :exclusions #{:all-pungs :outside-hand :pung-of-terminals-or-honors :no-honors}}

   #_#_:little-four-winds
       {:key :little-four-winds
        :name "Little four winds"
        :description "A hand that contains a pung/kong of three winds and a pair of the fourth."
        :points 64
        :predicate (every-pred
                    (having :pairs (same-family=n \w 1))
                    (having :pungs-or-kongs (same-family=n \w 3)))
        :exclusions #{:big-three-winds :pung-of-terminals-or-honors}}

   #_#_:little-three-dragons
       {:key :little-three-dragons
        :name "Little three dragons"
        :description "A hand that contains a pung/kong of two dragons and a pair of the third."
        :points 64
        :predicate (every-pred
                    (having :pairs (same-family=n \d 1))
                    (having :pungs-or-kongs (same-family=n \d 2)))
        :exclusions #{:dragon-pung :two-dragons}}

   #_#_:all-honors
       {:key :all-honors
        :name "All honors"
        :description "The pairs, Pungs or Kongs are all made up of Honor Tiles."
        :points 64
        :predicate (only-tiles :we :ws :ww :wn :dr :dg :dw)
        :exclusions #{:all-pungs :outside-hand :pung-of-terminals-or-honors}}

   #_#_:four-concealed-pungs
       {:key :four-concealed-pungs
        :name "Four concealed pungs"
        :description "Hand includes four Pungs achieved without melding"
        :points 64
        :predicate (having :concealed-pungs (nb= 4))
        :exclusions #{:fully-concealed :self-drawn}}

   #_#_:pure-terminal-chows
       {:key :pure-terminal-chows
        :name "Pure terminal chows"
        :description "Hand consists of two each of the lower and upper terminal Chows in one suit, with a pair of fivesin the same suit"
        :points 64
        :predicate (every-pred
                    (having :chows
                            (nb= 4)
                            (same-family)
                            (some-fn (at-least-one-of :b1 :b2 :b3 :b7 :b8 :b9)
                                     (at-least-one-of :c1 :c2 :c3 :c7 :c8 :c9)
                                     (at-least-one-of :s1 :s2 :s3 :s7 :s8 :s9)))
                    (having :pairs (at-least-one-of :b5)))
        :exclusions #{:seven-pairs
                      :full-flush
                      :all-chows
                      :pure-double-chow
                      :two-terminal-chows}}

   #_#_:quadruple-chow
       {:key :quadruple-chow
        :name "Quadruple chow"
        :description "Four chows of the same numerical sequences in the same suit."
        :points 48
        :predicate (having :chows (nb= 4) (all-equals))
        :exclusions #{:pure-shifted-pungs
                      :tile-hog
                      :pure-double-chow

                      ;; exclusions I added
                      :pure-triple-chow}}

   #_#_:four-pure-shifted-pungs
       {:key :quadruple-chow
        :name "Four pure shifted pungs"
        :description "Four Pungs or Kongs in the same suit, each shifted up one from the last."
        :points 48
        :predicate (having :pungs-or-kongs (nb= 4) (same-family) (shifted-family 1))
        :exclusions #{:pure-triple-chow :all-pungs}}

   #_#_:four-shifted-chows
       {:key :four-shifted-chows
        :name "Four shifted chows"
        :description "Four chows in one suit, each shifted up 1 or 2 numbers from the last, but not a combination of both."
        :points 32
        :predicate (having :chows (nb= 4) (same-family) (some-fn (shifted-family 1)
                                                                 (shifted-family 2)))
        :exclusions #{:short-straight}}

   #_#_:three-kongs
       {:key :three-kongs
        :name "Three kongs"
        :description "Hand contains three Kongs"
        :points 32
        :predicate (having :kongs (nb= 3))
        :exclusions #{}}

   #_#_:all-terminals-and-honors
       {:key :all-terminals-and-honors
        :name "All terminals and honors"
        :description "The pair(s), Pungs or Kongs are all made up of 1 or 9 Number Tiles and Honor Tiles."
        :points 32
        :predicate (every-pred (having :pungs-or-kongs (nb= 4))
                               (having :all-fans
                                       (at-least-one-of :b1 :b9 :c1 :c9 :s1 :s9)
                                       (at-least-one-of :dr :dg :dw :we :ws :ww :wn)))
        :exclusions #{}}

   #_#_:seven-pairs
       {:key :seven-pairs
        :name "Seven pairs"
        :description "Hand consisting of seven Pairs"
        :points 24
        :predicate (having :pairs (nb= 7))
        :exclusions #{:concealed-hand
                      :single-wait}}

   #_#_:greater-honors-and-knitted-tiles
       {:key :greater-honors-and-knitted-tiles
        :name "Greater honors and knitted tiles"
        :description "Formed by 7 single Honors (one of every Wind and Dragon), and singles of suit tiles belonging toseparate Knitted sequences (for example, 1-4-7 of Bamboos, 2-5-8 of Characters, and 3-6-9 of Stones)."
        :points 24
        :predicate (every-pred (having :knitted (nb= 1))
                               (every-tiles :wn :we :ws :ww :dr :dg :dw))
        :exclusions #{:concealed-hand
                      :all-types

                      ;; i added
                      :lesser-honors-and-knitted}}

   #_#_:all-even-pungs
       {:key :all-even-pungs
        :name "All even pungs"
        :description "A hand formed with Pungs of even-numbered suit tiles, and a pair of the same."
        :points 24
        :predicate (every-pred (having :pungs-or-kongs (nb= 4))
                               (having :all-fans (suits) (even)))
        :exclusions #{:all-pungs
                      :all-simple}}

   #_#_:full-flush
       {:key :full-flush
        :name "Full flush"
        :description "All the tiles are in the same suit."
        :points 24
        :predicate (having :all-fans (suits) (same-family))
        :exclusions #{:no-honors}}

   #_#_:pure-triple-chow
       {:key :pure-triple-chow
        :name "Pure triple chows"
        :description "Three chows of the same numerical sequence and in the same suit."
        :points 24
        :predicate (having :chows (all-equals 3))
        :exclusions #{:no-honors

                      ;; exclusions I added
                      :pure-shifted-pungs}}

   #_#_:pure-shifted-pungs
       {:key :pure-shifted-pungs
        :name "Pure shifted pungs"
        :description "Three Pungs or Kongs of the same suit, each shifted one up from the last."
        :points 24
        :predicate (having :pungs-or-kongs (shifted-family 3 1))
        :exclusions #{:pure-triple-chow}}

   #_#_:upper-tiles
       {:key :upper-tiles
        :name "Upper tiles"
        :description "Hand consisting entirely of 7, 8, and 9 tiles."
        :points 24
        :predicate (only-tiles :b7 :b8 :b9
                               :c7 :c8 :c9
                               :s7 :s8 :s9)
        :exclusions #{:no-honors
                      ;; I added
                      :upper-four}}

   #_#_:middle-tiles
       {:key :middle-tiles
        :name "Middle tiles"
        :description "Hand consisting entirely of 4, 5 and 6 tiles."
        :points 24
        :predicate (only-tiles :b4 :b5 :b6 :c4 :c5 :c6 :s4 :s5 :s6)
        :exclusions #{:no-honors
                      :all-simple}}

   #_#_:lower-tiles
       {:key :lower-tiles
        :name "Lower tiles"
        :description "Hand consisting entirely of 1, 2 and 3 tiles."
        :points 24
        :predicate (only-tiles :b1 :b2 :b3 :c1 :c2 :c3 :s1 :s2 :s3)
        :exclusions #{:no-honors
                      ;; I added
                      :lower-four}}

   #_#_:pure-straight
       {:key :pure-straight
        :name "Pure straight"
        :description "Hand using one of every number, 1-9, in three consecutive chows, in the same suit."
        :points 16
        :predicate (having :chows (shifted-family 3 3))
        :exclusions #{}}

   #_#_:three-suited-terminal-chows
       {:key :three-suited-terminal-chows
        :name "Three suited terminal chows"
        :description "Hand consisting of 1-2-3 + 7-8-9 in one suit (Two Terminal Chows), 1-2-3 + 7-8-9 in another suit,a pair of fives in the third suit."
        :points 16
        :predicate (having :all-fans
                           #{[[:chow :b1 :b2 :b3] [:chow :b7 :b8 :b9]
                              [:chow :c1 :c2 :c3] [:chow :c7 :c8 :c9]
                              [:pair :s5]]
                             [[:chow :c1 :c2 :c3] [:chow :c7 :c8 :c9]
                              [:chow :s1 :s2 :s3] [:chow :s7 :s8 :s9]
                              [:pair :b5]]
                             [[:chow :b1 :b2 :b3] [:chow :b7 :b8 :b9]
                              [:chow :s1 :s2 :s3] [:chow :s7 :s8 :s9]
                              [:pair :c5]]})
        :exclusions #{:pure-double-chow
                      :two-terminal-chows
                      :no-honors
                      :all-chows}}

   #_#_:pure-shifted-chows
       {:key :pure-shifted-chows
        :name "Pure shifted chows"
        :description "Three chows in one suit, each shifted up either one or two numbers from the last, but not acombination of both."
        :points 16
        :predicate (having :chows (some-fn (shifted-family 3 1) (shifted-family 3 2)))
        :exclusions #{}}

   #_#_:all-fives
       {:key :all-fives
        :name "All fives"
        :description "A hand in which every element includes a 5 tile"
        :points 16
        :predicate (having
                    :all-fans
                    (fn [fans]
                      (= 5 (count (keep (comp #(% 5) set #(map m/order %) rest) fans)))))
        :exclusions #{:all-simple}}

   #_#_:triple-pungs
       {:key :triple-pungs
        :name "Triple pungs"
        :description "Three Pungs of the same number, in each suit"
        :points 16
        :predicate (having :pungs-or-kongs (same-order-distinct-families 3))
        :exclusions #{}}

   #_#_:three-concealed-pungs
       {:key :three-concealed-pungs
        :name "Three concealed pungs"
        :description "Three Pungs achieved without melding"
        :points 16
        :predicate (having :concealed-pungs (nb= 3))
        :exclusions #{}}

   #_#_:lesser-honors-and-knitted
       {:key :lesser-honors-and-knitted
        :name "Lesser honors and knitted tiles"
        :description "A hand made of singles of the following tiles: Any Honors, along with Suit tiles that belong todifferent Knitted sequences (for example, 1-4-7 of Characters, 2-5-8 of Bamboos, and 3-6-9 ofDots - each of the 3 suits must belong to a different Knitted sequence, but not necessarily in theorder listed here)."
        :points 12
        :predicate (having :knitted (nb= 1))
        :exclusions #{}}

   #_#_:knitted-straight
       {:key :knitted-straight
        :name "Knitted straight"
        :description "A special Straight which is formed not with standard Chows but with 3 different Knittedsequences. For example, 1-4-7 of Dots, 2-5-8 of Characters, and 3-6-9 of Bamboos - but notnecessarily in this order"
        :points 12
        :predicate (having :knitted-straight (nb= 1))
        :exclusions #{}}

   #_#_:upper-four
       {:key :upper-four
        :name "Upper four"
        :description "A hand created solely with suit tiles 6 through 9"
        :points 12
        :predicate (only-tiles :b6 :b7 :b8 :b9
                               :c6 :c7 :c8 :c9
                               :s6 :s7 :s8 :s9)
        :exclusions #{:no-honors}}

   #_#_:lower-four
       {:key :lower-four
        :name "Lower four"
        :description "A hand created with suit tiles 1 through 4 only."
        :points 12
        :predicate (only-tiles :b1 :b2 :b3 :b4
                               :c1 :c2 :c3 :c4
                               :s1 :s2 :s3 :s4)
        :exclusions #{:no-honors}}

   #_#_:big-three-winds
       {:key :big-three-winds
        :name "Big three winds"
        :description "Hand includes Pungs or Kongs of three of the Winds."
        :points 12
        :predicate (having :pungs-or-kongs (same-family=n \w 3))
        :exclusions #{:no-honors}}
   })