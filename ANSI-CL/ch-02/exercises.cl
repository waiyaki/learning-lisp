;; Using car and cdr, define a function to return the fourth element of a list.
(defun my-fourth (lst)
  (car (cdr (cdr (cdr lst)))))

;; Define a function that takes two arguments and returns the greater of the two
(defun greater (x y)
  (if (> x y) x y))

;; Using only operators introduced in this chapter, define a function that
;; takes a list as an argument and returns true if one of its elements is a list
(defun has-list (lst)
  (if (null lst)
      nil
      (if (listp (car lst))
          t
          (has-list (cdr lst)))))

;; Give iterative and recursive definitions of a function that
;;    (a) takes a positive integer and prints that many dots.
;;    (b) takes a list and returns the number of times the symbol a occurs in it
(defun dot-times-recur (n)
  (format t ".")
  (if (> n 1) (dot-times-recur (- n 1))))

(defun dot-times-iter (n)
  (do ((i 0 (+ i 1)))
      ((>= i n) nil)
    (format t ".")))

(defun count-symbol-recur (sym lst &optional (count 0))
  (if (null lst)
      count
      (count-symbol-recur sym
                          (cdr lst)
                          (if (eql sym (car lst)) (+ 1 count) count))))

(defun count-symbol-iter (sym lst)
  (let ((count 0))
    (do ((i 0 (+ i 1)))
        ((>= i (length lst)) count)
      (if (eql (nth i lst) sym)
          (incf count)))))
