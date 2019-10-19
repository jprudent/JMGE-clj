(ns org.liprudent.majiang.fms)

(def game
  {:draw-a-tile            {:declare-mahjong        [:has-a-mahjong]
                            :declare-concealed-kong [:there-are-tiles-in-the-wall
                                                     :has-a-concealed-kong]
                            :replace-flower         [:there-are-tiles-in-the-wall
                                                     :has-flower]
                            :replace-season         [:there-are-tiles-in-the-wall
                                                     :has-season]
                            :discard-a-tile         [:always]}
   :declare-mahjong        {:win [:always]}
   :declare-concealed-kong {:draw-a-tile [:always]}
   :replace-flower         {:draw-a-tile [:always]}
   :replace-season         {:draw-a-tile [:always]}
   :discard-a-tile         {:idle [:always]}
   ;; todo splitter idle?
   :idle                   {:ready-to-auction [:current-player-has-discarded-a-tile]
                            :draw-a-tile      [:is-the-next-player
                                               :there-are-tiles-in-the-wall
                                               :nobody-eats-the-discarded-tile]
                            :loose            [????]}
   :ready-to-auction       {:want-mahjong [:has-a-mahjong]
                            :want-pung    [:there-are-tiles-in-the-wall
                                           :has-a-pair-in-hand]
                            :want-kong    [:there-are-tiles-in-the-wall
                                           :has-a-pung-in-hand]
                            :want-chow    [:there-are-tiles-in-the-wall
                                           :is-the-next-player
                                           :has-a-suit-of-2-in-hand]
                            :pass         [:always]}
   :want-mahjong           {:declare-mahjong [:has-highest-auction]
                            :idle            [:!has-highest-auction]}
   :want-pung              {:declare-pung [:has-highest-auction]
                            :idle         [:!has-highest-auction]}
   :declare-pung           {:draw-a-tile [:always]}
   :want-chaw              {:declare-chow [:has-highest-auction]
                            :idle         [:!has-highest-auction]}
   :declare-chow           {:draw-a-tile [:always]}})
