;;;; A DSL for writing SVG files.

;; Helper function for printing a single opening or closing XML tag.
(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (attributes)
          (format t " ~a=\"~a\"" (string-downcase (car attributes)) (cdr attributes)))
        alst)
  (princ #\>))

;; Tag macro for generating tags
(defmacro tag (name attrs &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                                     (pairs attrs)))
                     nil)
          ,@body
          (print-tag ',name nil t)))


;; Group items into a list of pairs
(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))

;; Macro for splitting a list
(defmacro split (val yes no)
  (let ((g (gensym)))
    `(let ((,g ,val))
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))

;; svg macro that embodies an entire SVG image
(defmacro svg (&body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
                   "xmlns:xlink" "http://www.w3.org/1999/xlink")
     ,@body))

;; Generate light/dark variants of a color.
;; 'color' is an RGB list. (255 0 0) is bright red.
(defun brightness (color amount)
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amount))))
          color))

;; Set the style of an SVG picture element
(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color
                  (brightness color -100))))

;; Draw a circle.
(defun circle (center radius color)
  (tag circle (cx (car center)
                  cy (cdr center)
                  r radius
                  style (svg-style color))))

;; Draw an arbitrary polygon
(defun polygon (points color)
  (tag polygon (points (format nil
                               "~{~a,~a ~}"
                               (mapcan (lambda (tp)
                                         (list (car tp) (cdr tp)))
                                       points))
                       style (svg-style color))))

;; Generate a random walk.
;; A random walk is a graph you'd get if you were to flip a coin and then
;; move either up or down a step.
(defun random-walk (value length)
  (unless (zerop length)
    (cons value
          (random-walk (if (zerop (random 2))
                           (1- value)
                           (1+ value))
                       (1- length)))))

;; Draw random walks in a picture
(defun draw-random-walks ()
  (with-open-file (*standard-output* "random_walk.svg"
                                     :direction :output
                                     :if-exists :supersede)
    (svg (loop repeat 10
            do (polygon (append '((0 . 200))
                                (loop for x from 0
                                   for y in (random-walk 100 400)
                                   collect (cons x y))
                                '((400 . 200)))
                        (loop repeat 3
                           collect (random 256)))))))

(defmacro svg-to-file (filename &body body)
  `(with-open-file (*standard-output* ,filename
                                      :direction :output
                                      :if-exists :supersede)
     ,@body))
