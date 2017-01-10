;; Utility to test if a number is prime using a simple, inefficient bruteforce approach.
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

;; Utility to get the next prime number greater than or equal to the argument.
(defun next-prime (number)
  (loop for n from number when (primep n) return n))

;; Macro do-primes that generates code that loops over only prime numbers
(defmacro do-primes-v1 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

(do-primes-v1 (p 0 19) (format t "~d " p))


;; do-primes-v2 - tries to fix a leak in v1 where the end subform is evaluated many times
(defmacro do-primes-v2 ((var start end) &body body)
  `(do ((ending-value ,end)
	(,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
     ,@body))

(do-primes-v2 (p 0 (random 100)) (format t "~d " p))


;; do-primes-v3 - fixes a leak introduced in v2 where the end form is evaluated before the start form.
(defmacro do-primes-v3 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	(ending-value ,end))
       ((> ,var ending-value))
     ,@body))

(do-primes-v3 (p 0 (random 100)) (format t "~d " p))


;; do-primes-v4 - fixes a leak introduced in v2 where we have a variable named "ending-value"
(defmacro do-primes-v4 ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))

;; Notice using ending-value-name as the variable name doesn't affect the running of the code.
(do-primes-v4 (ending-value-name 0 19) (format t "~d " ending-value-name))


;; do-primes-v5 - Use macros to write macros (macro that generates code that generates code 🤔
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes-v5 ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))
(do-primes-v5 (p 0 19) (format t "~d " p))
