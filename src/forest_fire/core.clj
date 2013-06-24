(ns forest-fire.core)

;; grid helper functions
;; ---------------------

(defn valid? 
  "determine if a space is valid"
  [grid [x y]]
  (let [width  (count (first grid))
        height (count grid)]
    (and (>= x 0) (>= y 0) (< x width) (< y height))))

(defn get-tile 
  "return the value of a square on a grid"
  [grid [x y]]
  (nth (nth grid y) x))

(defn neighbors
  "return the coordinates of all adjacent tiles"
  [grid [x y]]
  (let [delta-spaces [[-1 1] [0 1] [1 1] [-1 0] [1 0] [-1 -1] [0 -1] [1 -1]]
        surroundings (map #(map + [x y] %) delta-spaces)]
    (map #(get-tile grid %) (filter (partial valid? grid) surroundings))))

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
  (repeatedly height (fn [] (repeatedly width #(if (> 0.3 (rand)) :tree :empty)))))

(defn forest->vector
  "convert a forest to a vector for random access and assoc"
  [forest]
  (vec (map vec forest)))

(defn update-cell [forest [x y]]
  (case (get-tile forest [x y])
    :empty :empty
    :fire  :empty
    :tree (if ((set (neighbors forest [x y])) :fire) :fire (if (< 0.01 (rand)) :tree :fire))))

(defn update-cell-in-grid [grids cell]
  "takes a map with an old grid and a new grid to update, and a cell to update, and updates that cell"
  (assoc grids :new (assoc-in (:new grids) (reverse cell) (update-cell (:old grids) cell))))

(defn tick
  "return the successive forest"
  [forest]
  (:new (reduce update-cell-in-grid
           {:new (forest->vector forest) :old forest}
           (coords (count (first forest)) (count forest)))))
