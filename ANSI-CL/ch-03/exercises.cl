;; 2. Write a version of union that preserves the order of the elements in the
;;    original lists:
;;    > (new-union '(a b c) '(bad)) ; => (A B C D)
(defun new-union (list-a list-b)
  (if (not (and list-a list-b))
      (or list-a list-b)
      (new-union (if (not (member (car list-b) list-a))
                     (append list-a (list (car list-b)))
                     list-a)
                 (cdr list-b))))

;; 3. Define a function that takes a list and returns a list indicating the
;;    number of times each (eql) element appears, sorted from most common
;;    element to least common:
;;    > (occurrences '(a b a d a c d c e a))
;;    ; => ((A . 4) (D . 2) (C . 2) (B . 1) (E . 1))
(defun make-count-alist (item lst)
  (list (cons item (count item lst))))

(defun occurrences (lst)
  (let ((times nil))
    (dolist (item lst)
      (if (null times)
          (setf times (make-count-alist item lst))
          (if (not (assoc item times))
              (setf times (append times (make-count-alist item lst))))))
    (sort times #'> :key #'cdr)))

;; 5. Suppose the function pos+ takes a list and returns a list of each element
;;    plus its position:
;;    > (pos+ '(7514))
;;    ; => (7 6 3 7)
;;    Define this function using (a) recursion, (b) iteration, (c) mapcar.
(defun pos+recur (lst &optional (index 0) (pos nil))
  (if (null lst)
      (reverse pos)
      (pos+recur (cdr lst) (+ 1 index) (cons (+ index (car lst)) pos))))

(defun pos+iter (lst)
  (do ((i 0 (+ 1 i))
       (sums nil))
      ((>= i (length lst)) sums)
    (if (null sums)
        (setf sums (list (+ i (nth i lst))))
        (setf sums (append sums (list (+ i (nth i lst))))))))

(defun pos+mapcar (lst)
  (let ((index 0))
    (mapcar #'(lambda (x)
                (let ((sum (+ index x)))
                  (progn
                    (incf index)
                    sum)))
            lst)))
