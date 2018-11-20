(ns trikl.demo.world)

(defn tile [kind glyph color]
  {:tile/kind kind
   :entity/glyph glyph
   :entity/color color})

(def tiles
  {:floor (tile :floor "." :white)
   :wall  (tile :wall "#"  :white)
   :bound (tile :bound "X" :black)})

(defn get-tile [tiles x y]
  (get-in tiles [y x] (:bound tiles)))

(defn random-tile []
  (if (< (rand) 0.45)
    (:wall tiles)
    (:floor tiles)))

(defn random-tiles [cols rows]
  (vec (for [row (range rows)]
         (vec (for [col (range cols)]
                (assoc (random-tile) :entity/col col :entity/row row))))))

(defn neighbors [tiles x y]
  (let [sx (max 0 (dec x))
        sy (max 0 (dec y))]
    (for [px (range sx (+ sx 3))
          py (range sy (+ sy 3))]
      (get-in tiles [py px]))))

(defn tile-count [ts kind]
  (count (filter (comp #{kind} :tile/kind) ts)))

(defn smooth [ts]
  (vec
   (for [row (range (count ts))]
     (vec
      (for [col (range (count (get ts row)))]
        (let [tile (get-in ts [row col])
              neighbors (neighbors ts col row)
              wall-count (tile-count neighbors :wall)]
          (if (> wall-count 4)
            (merge tile (:wall tiles))
            (merge tile (:floor tiles)))))))))

(comment
  (defn inspect [tiles]
    (mapv (comp (partial apply str)
                (partial map :entity/glyph)) tiles))

  (def ts (random-tiles 160 50))
  [".##...##.#"
   ".#..###..."
   "#.##..#.#."
   "#..###...#"
   "....#...#."
   "#..#.#.##."
   "...##....#"
   "#.#.#.##.."
   ".#.##....#"
   "###..#.###"]

  (-> (random-tiles 10 10)
      smooth
      smooth
      smooth
      inspect)

  (tile-count
   (neighbors ts 2 1)
   :floor))
