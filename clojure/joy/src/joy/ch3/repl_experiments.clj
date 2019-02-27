(ns joy.ch3.repl-experiments)


(defn xors [max-x max-y]
  (for [x (range max-x)
        y (range max-y)]
    [x y (rem (bit-xor x y) 256)]))


(def frame (java.awt.Frame.))

(for [meth (.getMethods java.awt.Frame)
      :let [name (.getName meth)]
      :when (re-find #"Vis" name)]
  name)

(.isVisible frame)
(.setVisible frame true)

(.setSize frame (java.awt.Dimension. 200 200))

(def gfx (.getGraphics frame))

(.fillRect gfx 100 100 50 75)

(.setColor gfx (java.awt.Color. 255 128 0))
(.fillRect gfx 100 100 50 75)

(doseq [[x y xor] (xors 200 200)]
  (.setColor gfx (java.awt.Color. xor xor xor))
  (.fillRect gfx x y 1 1))


(defn clear [g]
  (.clearRect g 0 0 200 200))


(defn f-values [f xs ys]
  (for [x (range xs)
        y (range ys)]
    [x y (rem (f x y) 256)]))


(defn draw-values [f xs ys]
  (clear gfx)
  (.setSize frame (java.awt.Dimension. xs ys))
  (doseq [[x y v] (f-values f xs ys)]
    (.setColor gfx (java.awt.Color. v v v))
    (.fillRect gfx x y 1 1)))

(draw-values bit-and 256 256)

(draw-values + 256 256)

(draw-values * 256 256)

(draw-values (comp (partial apply bit-and) (juxt * +)) 256 256)
