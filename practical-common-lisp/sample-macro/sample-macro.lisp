;;;; Sample macro - take 2. See if I'll wrap my head around macros this time.

;;;; Create a macro that can loop over prime numbers
;;;; The macro should mirror other looping constructs like
;;;; do-times or do-list, meaning we'll use it as:
;;;; (do-primes (p 0 19) (format t "~d " p))

;; Helper function: prime-p
;; Test whether a number is prime
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

;; Helper function: next-prime
;; Return the next prime number greater or equal to a given number
(defun next-prime (number)
  (loop for n from number when (primep n) return n))


;; do-primes - version 1
;; Define macro do-primes with two parameters, one that holds the list
;; that contains the variable name, the lower bound and the upper bound and
;; another parameter that holds the body forms.
(defmacro do-primes-v1 (var-and-range &rest body)
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))


;; Macro parameters are 'destructuring' parameter lists, meaning you don't have
;; to take them apart by hand.
;; Can destructure by replacing the single parameter with a nested list parameter
;; list. The parameters in the nested list will take their values from the elements
;; that would have been bound to the parameter list replaced.
;; We can replace `var-and-range` with (var start end).
;; We can also use &body instead of &rest when defining macros. Semantically equivalent,
;; but editors indent uses of the macro with &body differently from &rest.
;; Destructuring parameter lists also give you automatic error checking since Lisp is
;; able to detect a call whose first argument isn't a 3 element list.
;; Destructuring parameter lists can also contain &optional, &key and &rest params.
(defmacro do-primes-v2 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

;; `do-primes-v2` evaluates the end form too many times.
;; That's a macro leak since in order to use do-primes correctly,
;; the user needs to be aware that the end form will be evaluated
;; multiple times.
;; To fix this, evaluate the end form and save the value for later use.
(defmacro do-primes-v3 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end))
       ((> ,var ending-value))
     ,@body))

;; New leak - `ending-value` parameter. See README for explanation
(defmacro do-primes-v4 ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

;; The generation of gensymed symbols to be used in the macros expansion
;; can be abstracted into a macro of its own. See README
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes-v5 ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))
