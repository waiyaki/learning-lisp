(ns reader.snake
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener))
  (:refer walkthrough.import-static :refer :all))

(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)

;; Set the size of the game board
(def width 75)
(def height 50)

;; Convert a game point into screen pixels.
(def point-size 10)

;; Control how many milliseconds pass before each game board update.
(def turn-millis 75)

;; The number of segments the snake needs to have to win the game.
(def win-length 5)

;; Map symbolic constants of the four directions to their vector equivalents
;; Use the VK_ constants defined in Swing instead of defining custom ones.
(def dirs {VK_LEFT  [-1 0]
           VK_RIGHT [1 0]
           VK_UP    [0 -1]
           VK_DOWN  [0 1]})


;; Add points to calculate the new position of moving a game object.
;; e.g move a point left by one:
;; (add-points [10 10] [-1 0]) ;; => [9, 10]
(defn add-points [& pts]
  (vec (apply map + pts)))

;; Convert a point in game space to a rectangle on the screen
;; (point-to-screen-rect [5 10]) ;; => (50 100 10 10)
(defn point-to-screen-rect [pt]
  (map #(* point-size %)
       [(pt 0) (pt 1) 1 1]))

;; Create a new apple
(defn create-apple []
  {:location [(rand-int width) (rand-int height)]
   :color (Color. 210 50 90)
   :type :apple})

;; Create a snake
(defn create-snake []
  {:body (list [1 1]) ; list of points
   :dir [1 0]         ; motion in this direction
   :type :snake
   :color (Color. 15 160 70)})


;; Move a snake.
;; Also grow the snake after eating an apple.
(defn move [{:keys [body dir] :as snake} & grow]
  (assoc snake
         :body
         (cons (add-points (first body) dir)
               (if grow body (butlast body)))))

;; Test whether a snake has won the game
(defn win? [{body :body}]
  (>= (count body) win-length))

;; Has the head come into contact with the body?
(defn head-overlaps-body? [{[head & body] :body}]
  (contains? (set body) head))

(def lose? head-overlaps-body?)

;; Eat an apple if the head occupies the apple's location
(defn eats? [{[snake-head] :body} {apple :location}]
  (= snake-head apple))

;; Turn the snake by updating its direction
(defn turn [snake newdir]
  (assoc snake :dir newdir))

;; Reset the game to initial state
(defn reset-game [snake apple]
  (dosync (ref-set apple (create-apple))
          (ref-set snake (create-snake)))
  nil)

(defn update-direction [snake newdir]
  (when newdir (dosync (alter snake turn newdir))))

;; Move the snake.
;; Grow the snake and create a new apple if the present one is eaten.
(defn update-positions [snake apple]
  (dosync
   (if (eats? @snake @apple)
     (do (ref-set apple (create-apple))
         (alter snake move :grow))
     (alter snake move)))
  nil)

;; Fill a single point
(defn fill-point [g pt color]
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g color)
    (.fillRect g x y width height)))


;; Paint snakes and apples
(defmulti paint (fn [f object & _] (:type object)))

(defmethod paint :apple [g {:keys [location color]}]
  (fill-point g location color))

(defmethod paint :snake [g {:keys [body color]}]
  (doseq [point body]
    (fill-point g point color)))

;; Create a Swing JPanel for handling painting the game, updating on each timer
;; tick and responding to use input.
(defn game-panel [frame snake apple]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (paint g @snake)
      (paint g @apple))
    (actionPerformed [e]
      (update-positions snake apple)
      (when (lose? @snake)
        (reset-game snake apple)
        (JOptionPane/showMessageDialog frame "You lose!"))
      (when (win? @snake)
        (reset-game snake apple)
        (JOptionPane/showMessageDialog frame "You win!"))
      (.repain this))
    (keyPresssed [e]
      (update-direction snake (dirs (.getKeyCode e))))
    (getPreferredsize []
      (Dimension. (* (inc width) point-size)
                  (* (inc height) point-size)))
    (keyReleased [e])
    (keyTyped [e])))

;; Create a new game.
(defn game []
  (let [snake (ref (create-snake))
        apple (ref (create-apple))
        frame (JFrame. "Snake")
        panel (game-panel frame snake apple)
        timer (Timer. turn-millis panel)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    (.start timer)
    [snake apple timer]))
