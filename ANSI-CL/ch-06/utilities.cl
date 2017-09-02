(defun single? (lst)
  "Return true if argument is a list of one element."
  (and (consp lst) (null (cdr lst))))

(defun append1 (lst obj)
  "Like cons but adds an element to the end of the list instead of to the front."
  (append lst (list obj)))

(defun map-int (fn n)
  "Return a list of the results of calling function fn n times."
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

(defun filter (fn lst)
  "Return a list containing only the values for which fn returns true."
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push x acc))))
    (nreverse acc)))

(defun most (fn lst)
  "Return the element of a list with the highest score, based on some scoring function."
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf wins obj
                    max score))))
        (values wins max))))
