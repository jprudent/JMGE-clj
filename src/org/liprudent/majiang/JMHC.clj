(comment "TODO implement other actions and do this at the end"
  (import
    '(org.liprudent.majiang HuFinder)
    '(org.liprudent.majiang.tiles PlayerTiles))
(ns commands.clj
  (:import [org.liprudent.majiang HuFinder]
           [org.liprudent.majiang.tiles PlayerTiles PlayerContext ConcealedTiles WindFamily$ EastWind NorthWind WestWind SouthWind TileSet Tile$]
           [scala.collection.immutable List List$ $colon$colon]))

(def mapWind { :east EastWind
               :north NorthWind
               :west WestWind
               :south SouthWind })

(defn- str-no-cols [k] (subs (str k) 1))

(defn to-jmhc-tile [tile]
  (.apply (org.liprudent.majiang.tiles.Tile$/MODULE$) (str-no-cols tile) ))

(defn to-scala-list [& vals]
  (reduce
    #(.apply scala.collection.immutable.$colon$colon$/MODULE$ %2 %1)
    (scala.collection.immutable.Nil$/MODULE$)
    (map to-jmhc-tile vals)))

(defn to-tileset [& tiles]
  (.apply (org.liprudent.majiang.tiles.TileSet$/MODULE$) (apply to-scala-list tiles)))

(defn get-last-tile )

(defn hule-details [game player]
  (let [playerContext (. PlayerContext (player mapWind) (get-prevalent-wind game))
        lastTile (to-jmhc-tile (get-last-discarded ))
        lastTileContext ()
        concealedTiles (.apply org.liprudent.majiang.tiles.ConcealedTiles$/MODULE$ playerTiles lastTileContext)
        playerTiles (. PlayerTiles)
        huFinder (. HuFinder playerTiles playerContext)]))

)