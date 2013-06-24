(ns forest-fire.render
  (:use forest-fire.core)
  (:import java.awt.image.BufferedImage java.awt.Color java.io.File javax.imageio.ImageIO))

(def *HEIGHT* 100)
(def *WIDTH* 100)

(def *image* (new BufferedImage *HEIGHT* *WIDTH* (. BufferedImage TYPE_INT_RGB)))

(defn set-color!
  "iterates over an image with a function that takes coordinates and returns colors"
  [image color-chooser]
  (doseq [x (range (.getWidth image)) y (range (.getHeight image))]
    (.setRGB image x y (color-chooser x y)))) 

(defn forest->image [forest]
  (let [height (count forest) width (count forest) image (new BufferedImage height width (. BufferedImage TYPE_INT_RGB))]
    (set-color! image (fn [x y] (.getRGB (case (get-tile forest [x y]) :tree (Color/green) :empty (Color/white) :fire (Color/red)))))
    image))

(defn write-file [image filename]
  (ImageIO/write image "png" (new File filename)))

(defn -main [& args]
  (set-color! *image* (fn [x y] -1))
  (write-file *image* "white-square.png"))
