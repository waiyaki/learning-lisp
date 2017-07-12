;; Library for adding lazy evaluation to Lisp
(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))

;; Force evaluation of a lazy function by simply calling the function 😅
(defun force (lazy-value)
  (funcall lazy-value))

;;; Lazy lists.
(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

;; Lazy nil to terminate a list
(defun lazy-nil ()
  (lazy nil))

;; Lazy null to check if we've reached the end of a list.
(defun lazy-null (x)
  (not (force x)))

;; Convert a regular list to a lazy list
(defun make-lazy (lst)
  (lazy (when lst
          (cons (car lst) (make-lazy (cdr lst))))))

;; Convert a lazy list to a regular list
(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

;; Map across lazy lists
(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fun (lazy-car lst))
                (lazy-mapcar fun (lazy-cdr lst))))))

(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur)
             (if (lazy-null lst-cur)
                 (force (lazy-mapcan fun (lazy-cdr lst)))
                 (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null lst)
            (f (funcall fun (lazy-car lst)))))))

;; Find in lazy lists
(defun lazy-find-if (fn lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fn x)
          x
          (lazy-find-if fn (lazy-cdr lst))))))

(defun lazy-nth (n lst)
  (if (zerop n)
      (lazy-car lst)
      (lazy-nth (1- n) (lazy-cdr lst))))
