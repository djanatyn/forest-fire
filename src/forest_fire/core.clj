(ns forest-fire.core)

;; grid helper functions
;; ---------------------

(defn valid? 
  "determine if a space is valid"
  [grid [x y]]
  (let [width  (count (first grid))
        height (count grid)]
    (and (>= x 0) (>= y 0) (< x width) (< y height))))

(defn neighbors
  "return the coordinates of all adjacent tiles"
  [grid [x y]]
  (let [delta-spaces [[-1 1] [0 1] [1 1] [-1 0] [1 0] [-1 -1] [0 -1] [1 -1]]
        surroundings (map #(map + [x y] %) delta-spaces)]
    (map #(get-tile grid %) (filter (partial valid? grid) surroundings))))


(defn get-tile 
  "return the value of a square on a grid"
  [grid [x y]]
  (nth (nth grid y) x))

(defn coords
  "return all the possible coordinates with a certain width and height"
  [width height]
  (mapcat (fn [x] (map (fn [y] [x y]) (range height))) (range width)))

(defn replace-tile
  "replace a value on a grid with another and return the new grid"
  [grid [x y] value]
  (assoc grid y (assoc (nth grid y) x value)))

;; generate forest
;; ---------------

(defn gen-forest
  "generate a forest of specified width and height"
  [width height]
  (repeatedly height (fn [] (repeatedly width #(if (> 0.5 (rand)) :tree :empty)))))

(defn forest->vector
  "convert a forest to a vector for random access and assoc"
  [forest]
  (vec (map vec forest)))

(defn update-cell [forest [x y]]
  (case (get-tile forest [x y])
    :empty :empty
    :fire  :empty
    :tree (if ((set (neighbors forest [x y])) :fire) :fire :tree)))

(defn tick
  "return the successive forest"
  [forest]
  (loop [new-forest (forest->vector forest)
         old-forest forest
         coords-left (coords (count (first forest)) (count forest))]
    (let [current-coord (first coords-left)
          tile (get-tile old-forest current-coord)]
      (if (empty? coords-left) new-forest
          (recur (assoc-in new-forest current-coord (update-cell old-forest current-coord))
                 old-forest
                 (rest coords-left))))))
