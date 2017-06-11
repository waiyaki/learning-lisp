;; Our game world is extremely simple.
;; It consists of a simple rectangular plane, with edges that wrap around
;; to the opposite side (Mathematically speaking, it has a toroidal topology.)
;; Most of this world is covered in steppes, meaning that very few plants grow
;; for the animals to eat. In the center of the world is a small jungle,
;; where plants grow much faster. Our animals, who are herbivores,
;; will forage this world in search for food.

(defparameter *width* 100
  "Width of the world.")

(defparameter *height* 30
  "Height of the world.")

(defparameter *jungle* '(45 10 10 10)
  "The rectangle in the world that contain the jungle. ((top-left corner x, y), width, height)")

(defparameter *plant-energy* 80
  "Energy contained in each plant (if eaten, eater gains 80 days of food).")

(defparameter *plants* (make-hash-table :test #'equal))


(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

;;; Animals
;; x, y - animal location in x, y coordinates
;; energy - amount of energy days an animal has remaining. Otherwise will starve and die.
;; dir - Direction animal is facing, animal's next x, y position from 0 - 7
;; genes - Gene slots (1 - 8) random choice of a new direction.
(defstruct animal x y energy dir genes)

(defparameter *animals*
  (list (make-animal :x (ash *width* -1)
                     :y (ash *height* -1)
                     :energy 1000
                     :dir 0
                     :genes (loop repeat 8
                               collecting (1+ (random 10))))))

;; Move an animal orthogonally or diagonally based on animal direction
(defun move (animal)
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
                                    (cond ((and (>= dir 2) (< dir 5)) 1)
                                          ((or (= dir 1) (= dir 5)) 0)
                                          (t -1))
                                    *width*)
                                 *width*))
    (setf (animal-y animal) (mod (+ y
                                    (cond ((and (>= dir 0) (< dir 3)) -1)
                                          ((and (>= dir 4) (< dir 7)) 1)
                                          (t 0))
                                    *height*)
                                 *height*))
    (decf (animal-energy animal))))


;; Use animal genes to decide if and how much to turn on a given day
(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
               (let ((xnu (- x (car genes))))
                 (if (< xnu 0)
                     0
                     (1+ (angle (cdr genes) xnu))))))
      (setf (animal-dir animal)
            (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
                 8)))))

;; Make the animal eat.
(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))


;; Handle animal reproducing
(defparameter *reproduction-energy* 200)

(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu (copy-structure animal))
            ;; copy-list here to copy genes, since copy-structure only performs a shallow copy
            (genes (copy-list (animal-genes animal)))
            (mutation (random 8)))
        (setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*)))))


;; Simulate a day in this world
(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
                               (<= (animal-energy animal) 0))
                             *animals*))
  (mapc (lambda (animal)
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  (add-plants))


;; Drawing the world.
(defun draw-world ()
  (loop for y
     below *height*
     do (progn (fresh-line)
               (princ "|")
               (loop for x
                  below *width*
                  do (princ (cond ((some (lambda (animal)
                                           (and (= (animal-x animal) x)
                                                (= (animal-y animal) y)))
                                         *animals*)
                                   #\M)
                                  ((gethash (cons x y) *plants*) #\*)
                                  (t #\space))))
               (princ "|"))))

;; User Interface
(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
          (t (let ((x (parse-integer str :junk-allowed t)))
               (if x
                   (loop for i
                      below x
                      do (update-world)
                      if (zerop (mod i 1000))
                      do (princ #\.)
                        (force-output))
                   (update-world))
               (evolution))))))
