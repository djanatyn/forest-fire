(ns forest-fire.render
  (:use forest-fire.core)
  (:import java.awt.image.BufferedImage java.awt.Color java.io.File javax.imageio.ImageIO)
  (:gen-class))

(defn set-color!
  "iterates over an image with a function that takes coordinates and returns colors"
  [image color-chooser]
  (doseq [x (range (.getWidth image)) y (range (.getHeight image))]
    (.setRGB image x y (color-chooser x y)))) 

(defn forest->image [forest]
  (let [height (count (first forest)) width (count forest) image (new BufferedImage height width (. BufferedImage TYPE_INT_RGB))]
    (set-color! image (fn [x y] (.getRGB (case (get-tile forest [x y]) :tree (Color/green) :empty (Color/white) :fire (Color/red)))))
    image))

(defn write-file [image filename]
  (ImageIO/write image "png" (new File filename)))

(defn -main [& args]
  (let [timeline (iterate tick (gen-forest 100 100))]
    (doseq [n (range 50)] (write-file (forest->image (nth timeline n)) (str "out/gen" n ".png")))))
