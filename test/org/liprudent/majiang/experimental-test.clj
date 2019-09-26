(scenario "A player say HuLE"
  (given :east has-tiles [:b3 :s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s9 :s1 :s2 :s3 ])
  (given :north has-tiles [:b4 :b5 :s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s9 :s1 :s2 ])
  (given :west has-tiles  [:b3 :b3 :b5 :b5 :b5 :b6 :b6 :b6 :b7 :b7 :b7 :b8 :b8 ])
  (given :east :pass))
