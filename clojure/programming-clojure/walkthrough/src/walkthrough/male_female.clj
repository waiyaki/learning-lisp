;;;; Hofstadter Female Male Sequences
(declare m f)

;; Mutually recursive definition
;; Very slow
(defn m [n]
  (if (zero? n)
    0
    (- n (f (m (dec n))))))

(defn f [n]
  (if (zero? n)
    1
    (- n (m (f (dec n))))))

;; Memoized version. Faster than previous defition
;; Needs the cache to have been built, else it will blow the stack
(def m (memoize m))
(def f (memoize f))

;; Exposing sequences instead of functions to ensure the cache is built from ground up
(def m-seq (map m (iterate inc 0)))
(def f-seq (map f (iterate inc 0)))
