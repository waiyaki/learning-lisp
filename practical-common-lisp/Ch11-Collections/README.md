# Collections
- Apart from lists, Lisp has `vectors` and `hashtables`.
- Lists and Vectors share enough characteristics that Common Lisp treats them as subtypes of the `sequence` abstraction.

## Vectors
- Common Lisp's basic integer-indexed collection.
- Of two flavours:
    - Fixed-size vectors - a thin veneer over a chunk of contiguous memory that holds the vector's elements. Lot like arrays in Java
    - Resizable vectors - These abstract the storage, allowing the vector to grow and shrink as elements are added and removed. Analogous to lists in Python.

The literal notation syntax used for vectors is `#(...)`, which allows for saving and restoring vectors by printing them and reading them back in.

#### `vector`
Fixed-size vectors containing specific values are made using the `vector` function:
```lisp
(vector)    ; => #()
(vector 1)  ; => #(1)
(vector 1 2) ; => #(1 2)
```

#### `make-array`
- For vectors that are to be modified, use the `make-array` function.
- Can be used to create arrays of any dimensionality as well as fixed-size and resizable vectors.
- Requires a list argument specifying the dimensions of the array.
- Also accepts a plain number in place of a one item list for convenience.
- With no other arguments, `make-array` will create a vector with uninitialized elements that must be set before they can be accessed.
- To initialize a vector with all elements set to a particular value, use an `:initial-element` argument:
```lisp
(make-array 5 :initial-element nil)   ; => #(NIL NIL NIL NIL NIL)
```

Resizable vectors are more complex than fixed-size vectors since in addition to keeping track of the memory used to hold the elements and the number of slots available, a resizable vector also keeps track of the number of elements actually stored in the vector. This number is stored in the vector's `fill-pointer`. `fill-pointer` points to the next position to be filled when you add an element into the vector.

To create a vector with a fill-pointer, pass `make-array` a `:fill-pointer` argument.
```lisp
; Looks empty since the fill pointer is zero.
(make-array 5 :fill-pointer 0)  ; => #()
```

- Use `vector-push` to add an element to the end of a resizable vector. Adds the element a the current value of the fill pointer, increments the fill pointer and returns the index where the new element was added.
- `vector-pop` - Returns most recently pushed item, decrementing the fill pointer in the process.

```lisp
(defparameter *x* (make-array 5 :fill-pointer 0))

(vector-push 'a *x*) ; => 0
*x*                  ; => #(A)
(vector-push 'b *x*) ; => 1
*x*                  ; => #(A B)
(vector-push 'c *x*) ; => 2
*x*                  ; => #(A B C)
(vector-pop *x*)     ; => C
*x*                  ; => #(A B)
(vector-pop *x*)     ; => B
*x*                  ; => #(A)
(vector-pop *x*)     ; => A
*x*                  ; => #()
```

- To create an arbitrarily resizable vector, pass `make-array` an `:adjustable` kwarg.
```lisp
(make-array 5 :fill-pointer 0 :adjustable t) ; => #()
```
- Use `vector-push-extend` to add elements to an adjustable vector. Works like `vector-push` except it'll automatically expand the array if push an element to a full vector.

## Vectors as Sequences
- `length` - return the length of a sequence.
- `elt` - allow access to an element of a sequence via an integer index. `elt` is `setf`able.

```lisp
(defparameter *x* (vector 1 2 3))

(length *x*) ; => 3
(elt *x* 0)  ; => 1
(elt *x* 1)  ; => 2
(elt *x* 2)  ; => 3
(elt *x* 3)  ; => error

; using elt with setf
(setf (elt *x* 0) 10) ; => #(10 2 3)
```
### Sequence Iterating Functions
#### Basic Sequence Functions

| Name       | Required Arguments           | Returns                                  |
| ---------- | ---------------------------- | :--------------------------------------- |
| COUNT      | Item and sequence            | Number of times item appears in sequence |
| FIND       | Item and sequence            | Item or NIL                              |
| POSITION   | Item and sequence            | Index into sequence or NIL               |
| REMOVE     | Item and sequence            | Sequence with instances of item removed  |
| SUBSTITUTE | New item, item, and sequence | Sequence with instances of item replaced with new item |

```lisp
(count 1 #(1 2 1 2 3 1 2 3 4))         ; => 3
(remove 1 #(1 2 1 2 3 1 2 3 4))        ; => #(2 2 3 2 3 4)
(remove 1 '(1 2 1 2 3 1 2 3 4))        ; => (2 2 3 2 3 4)
(remove #\a "foobarbaz")               ; => "foobrbz"
(substitute 10 1 #(1 2 1 2 3 1 2 3 4)) ; => #(10 2 10 2 3 10 2 3 4)
(substitute 10 1 '(1 2 1 2 3 1 2 3 4)) ; => (10 2 10 2 3 10 2 3 4)
(substitute #\x #\b "foobarbaz")       ; => "fooxarxaz"
(find 1 #(1 2 1 2 3 1 2 3 4))          ; => 1
(find 10 #(1 2 1 2 3 1 2 3 4))         ; => NIL
(position 1 #(1 2 1 2 3 1 2 3 4))      ; => 0
```

- `remove` and `substitute` will always return a sequence of the same type as their sequence argument.

#### Standard Sequence Function Keyword Arguments
The behaviour of these functions can be modified using keyword arguments:
- `:test` - Used to pass a function that accepts two arguments and returns a boolean. Used to compare _item_ to each element instead of the default comparison function, `eql`.
```lisp
  (count "foo" #("foo" "bar" "baz") :test #'string=) ; ==> 1
```
- `:key` - Used to pass a one-argument function that is called on each element to extract the key that will then be compared with _item_ in place of the element itself. Functions such as `find` return the actual element and not just the extracted key.
```lisp
  (find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first) ; => (C 30)
```

-   `:start` and `:end` - limit the functions to a subsequence of the sequence. Providing `nil` `:end` is the same a specifying the length of the sequence.

-   `:from-end` - A non-nil `from-end` kwarg makes the elements of the sequence to be examined from the end. `from-end` alone affects the results of `find` and `position`.

-   `count` - Used to specify the number of elements on which to operate. When used with `from-end`, makes `from-end` affect the results of `remove` and `substitute`.

    ```lisp
      (find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first)             ; => (A 10)
      (find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t) ; => (A 30)

      (remove #\a "foobarbaz" :count 1)             ; => "foobrbaz"
      (remove #\a "foobarbaz" :count 1 :from-end t) ; => "foobarbz"
    ```


| Argument    | Meaning                                  | Default |
| ----------- | ---------------------------------------- | ------- |
| `:test`     | Two-argument function used to compare item (or value extracted by `:key` function) to element. | EQL     |
| `:key`      | One-argument function to extract key value from actual sequence element. NIL means use element as is. | NIL     |
| `:start`    | Starting index (inclusive) of subsequence | 0       |
| `:end`      | Ending index (exclusive) of subsequence. NIL indicates end of sequence. | NIL     |
| `:from-end` | If true, the sequence will be traversed in reverse order, from end to start | NIL     |
| `:count`    | Number indicating the number of elements to remove or substitute or NIL to indicate all (REMOVE and SUBSTITUTE only) | NIL     |

### Higher Order Function Variants

For each of the basic sequence iterating functions, Common Lisp provides two higher-order function variants that, in place of an item argument, take in a function to be called on each element of the sequence.

- One set of variants are named the same as the basic version with an `-if` appended. These functions operate on elements for which the function returns true.
- The other set are named with an `if-not` suffix and operate on elements for which the function does **not** return true.

  ```lisp
  (count-if #'evenp #(1 2 3 4 5))         ; => 2

  (count-if-not #'evenp #(1 2 3 4 5))     ; => 3

  (position-if #'digit-char-p "abcd0001") ; => 4

  (remove-if-not #'(lambda (x) (char= (elt x 0) #\f))
    #("foo" "bar" "baz" "foom")) ; => #("foo" "foom")
  ```

- `remove` family also supports a fourth variant, `remove-duplicates` which has one required argument, the sequence, from which it removes all but one occurrence of a particular element. Takes same keywords as `remove` except for `:count`, since it removes all duplicates.
  ```lisp
  (remove-duplicates #(1 2 1 2 3 1 2 3 4)) ; => #(1 2 3 4)
  ```

The `-if` and `-if-not` variants also accept all the key arguments as their basic counterparts except for `:test`, which isn't needed since the main argument is already a function. With the `:key` argument, the value extracted by the `:key` function is passed to the function instead of the actual element.
```lisp
(count-if #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first)     ; => 2

(count-if-not #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first) ; => 3

(remove-if-not #'alpha-char-p
  #("foo" "bar" "1baz") :key #'(lambda (x) (elt x 0))) ; => #("foo" "bar")
```
### Whole Sequence Manipulations
- `copy-seq` - Take in a single argument. Return sequence of the same type as argument sequence.

- `reverse` - Single argument. Return the sequence in reverse.

- `concatenate` - Creates a new sequence containing the concatenation of any number of sequences. Must specify what type of sequence to return. It's first argument is a type descriptor. Most likely type descriptors are `vector`, `list` and `string`.
  ```lisp
  (concatenate 'vector #(1 2 3) '(4 5 6))     ; => #(1 2 3 4 5 6)
  (concatenate 'list #(1 2 3) '(4 5 6))       ; => (1 2 3 4 5 6)
  (concatenate 'string "abc" '(#\d #\e #\f))  ; => "abcdef"
  ```

### Sorting and Merging
- `sort` and `stable-sort` provide 2 ways of sorting sequences. Both take a sequence and a two-argument predicate and return a sorted version of the sequence. `stable-sort` guarantees not to reorder elements considered equivalent while `sort` may reorder elements considered equivalent.

- Both sorting functions are *_destructive_* functions. Destructive functions are allowed to modify their arguments in arbitrary way for efficiency reasons.
  1. Use the return value of the functions
  2. Always pass a copy of the object unless you're done with it.

- Both functions take a `:key` keyword argument, which should be a function that will be used to extract the values passed to the sorting predicate.

- `merge` takes two sequences and a predicate and returns a sequences produced by merging the two sequences according to the predicate. Like `sort`, `merge` takes a `:key` kwarg. Like `concatenate`, the first argument to `merge` must be a type descriptor.
  ```lisp
  (merge 'vector #(1 3 5) #(2 4 6) #'<) ; => #(1 2 3 4 5 6)
  (merge 'list #(1 3 5) #(2 4 6) #'<)   ; => (1 2 3 4 5 6)
  ```

### Subsequence Manipulations
- `subseq` - Extracts a subsequence starting at a particular index and continuing to a particular index or the end of the sequence.
  ```lisp
  (subseq "foobarbaz" 3)    ; => "barbaz"
  (subseq "foobarbaz" 3 6)  ; => "bar"
  ```
  `subseq` is `setf`able but it doesn't extend or shrink a sequence. If the new value and the subsequence to be replaced are different lengths, the shorter of the two determines how many characters are actually changed.
  ```lisp
  ; SLIME 2015-08-14
  CL-USER> (defparameter *x* (copy-seq "foobarbaz"))
  *X*
  CL-USER> (setf (subseq *x* 3 6) "xxx") ; subseq and new value same length
  "xxx"
  CL-USER> *x*
  "fooxxxbaz"
  CL-USER> (setf (subseq *x* 3 6) "abcd") ; subseq too short, extra new characters ignored.
  "abcd"
  CL-USER> *x*
  "fooabcbaz"
  CL-USER> (setf (subseq *x* 3 6) "xx") ; subseq too long, only 2 characters are changed.
  "xx"
  CL-USER> *x*
  "fooxxcbaz"
  CL-USER>
  ```

- Can use `fill` to set multiple elements of a sequence to a single value. Required arguments are the sequence and the value with which to fill it. Fills every element by default, `:start` and `:end` can be used to limit that.

- `search` finds subsequence within a sequence. Works like `position` except it's first argument is a sequence instead of a single item.
  ```lisp
  (position #\b "foobarbaz") ; => 3
  (search "bar" "foobarbaz") ; => 3
  ```

- `mismatch` - Used to find where two sequences diverge. Takes two sequences and returns the index of the first pair of mismatched elements. Also accepts `:key`, `:test`, `:start1`, `:end1`, `:start1`, `:end1` and `:from-end`.
  ```lisp
  CL-USER> (mismatch "foobarbaz" "foom")
  3
  CL-USER> (mismatch "foobar" "bar" :from-end t)
  3
  CL-USER>
  ```

### Sequence Predicates
- `some`, `every`, `notany` and `notevery` iterate over sequences testing a boolean predicate. First argument is the test predicate, the rest are the sequences. Predicate function should take as many arguments as the number of sequences passed. The elements are passed to the predicate, an element from each sequence, until one of the sequences runs out of elements or the overall termination test is met.
- `every` - Terminates returning false as soon as the predicate fails.
- `some` - Returns the first non-`nil` element returned by the predicate or false if the predicate is never satisfied.
- `notany` - Returns false as soon as the predicate is satisfied or true if it never is.
- `notevery` - Returns true as soon as the predicate fails or false if the predicate is always satisfied.

  ```lisp
  CL-USER> (every #'evenp #(1 2 3 4 5))
  NIL
  CL-USER> (some #'evenp #(1 2 3 4 5))
  T
  CL-USER> (notany #'evenp #(1 2 3 4 5))
  NIL
  CL-USER> (notevery #'evenp #(1 2 3 4 5))
  T
  CL-USER> ; Compare elements of two sequences pairwise
  ; No value
  CL-USER> (every #'> #(1 2 3 4) #(5 4 3 2))
  NIL
  CL-USER> (some #'> #(1 2 3 4) #(5 4 3 2))
  T
  CL-USER> (notany #'> #(1 2 3 4) #(5 4 3 2))
  NIL
  CL-USER> (notevery #'> #(1 2 3 4) #(5 4 3 2))
  T
  CL-USER>
  ```

### Sequence Mapping functions.
- `map` - Takes a *n*-argument predicate and *n* sequences. Returns a new sequence containing the result of applying the function to subsequent elements of the sequences. Like `concatenate` and `merge`, `map` needs to be told what type of sequence to create.
  ```lisp
  CL-USER> (map 'vector #'* #(1 2 3 4 5) #(10 9 8 7 6))
  #(10 18 24 28 30)
  CL-USER>
  ```

- `map-into` - Like `map`, but instead of producing a new sequence, it places the results into a sequence passed as the first argument. The sequence can be the same as one of the sequences providing values for the function. e.g., to sum the elements of several vectors:
  ```lisp
  CL-USER> (let ((a #(1 2 3)) (b #(4 5 6)) (c #(7 8 9)))
              (map-into a #'+ a b c))
  #(12 15 18)
  CL-USER>
  ```
  `map-into` only affects as many elements as are present in the shortest sequence, including the sequence being mapped into.

`reduce` - Maps over a single sequence, applying a two argument function to the first two elements of the sequence first, then to the result of that computation and the third element and so on for all the remaining elements of the sequence. In essence, it *reduces* a sequence into a single element.
  ```lisp
  (reduce #'+ #(1 2 3 4 5 6 7 8 9 10)) ; => 55
  ```
  Also takes the full complement of keyword arguments: `:key`, `:from-end`, `:start`, `:end` and `:initial-value`, which specifies a value that'll be logically placed before the first element of the sequence, or the last element if you specify `:from-end t`.

## Hash Tables
- Hash tables allow you to use arbitrary objects as the keys (indices).
- Comparable to dictionaries in Python, Objects in JavaScript.
- Create a hash table using `make-hash-table`.
- With no arguments, `make-hash-table` creates a hash table that considers two keys equivalent if they're the same object according to `eql`.
- If you want to use strings as keys, create an `equal` hash table by passing the symbol `equal` as the `:test` kwarg to `make-hash-table`. Other possible values for `:test` are `eq` and `equalp`. Unlike the `:test` kwarg in sequences, this one can't be used to pass in arbitrary functions, just `:eq`, `eql`, `equal` and `equalp`. This is because hash tables actually need two functions, an equivalence function and a hash function that computes a numerical hash code from the key in a way compatible with how the equivalence function will ultimately compare two keys.
- `gethash` - Provides access to the elements of a hash table. Takes 2 arguments, a key and the hash table and returns a value, if any, stored in the hash table under the provided key or `nil`.

  ```lisp
  CL-USER> (defparameter *h* (make-hash-table))
  *H*
  CL-USER> (gethash 'foo *h*)
  NIL
  NIL
  CL-USER> (setf (gethash 'foo *h*) 'quux)
  QUUX
  CL-USER> (gethash 'foo *h*)
  QUUX
  T
  CL-USER>
  ```
  Since `gethash` returns `nil` when the key isn't present in the hash table, a feature to distinguish missing keys from keys with `nil` as their value is required. This feature is *_multiple_* return values. The primary value is the value stored under the key. The secondary value is a boolean indicating whether the key is present in the table. Can use the macro `multiple-value-bind` to take advantage of `gethash`'s extra return value. `multiple-value-bind` creates a binding like `let` does, filling them with the multiple values returned from a form.
  ```lisp
  CL-USER> (defun show-value (key hash-table)
           (multiple-value-bind (value present) (gethash key hash-table)
             (if present
                 (format nil "Value ~a actually present." value)
                 (format nil "Value ~a because key not found." value))))
  SHOW-VALUE
  CL-USER> (setf (gethash 'bar *h*) nil) ; Provide explicit value for nil
  NIL
  CL-USER> (show-value 'foo *h*)
  "Value QUUX actually present."
  CL-USER> (show-value 'bar *h*)
  "Value NIL actually present."
  CL-USER> (show-value 'baz *h*)
  "Value NIL because key not found."
  CL-USER>
  ```
- `remhash` - Takes same args as `gethash`, removes the specified entry.
- `clrhash` - Completely clears a hash table of all its key-value pairs.

### Hash Table Iteration
- `maphash` - Analogous to `map`, `maphash` takes a two argument function and a hash table and invokes the function once for each key/value pair in the hash table. e.g., to print all the key/value pairs in a hashtable:
  ```lisp
  (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *h*)
  ```
  Consequences of adding/removing elements from a hash table while iterating over it aren't defined. However, can use `setf` with `gethash` to change the value of the current entry or `remhash` to remove the current entry. e.g, to remove all entries whose value is less than 10:
  ```lisp
  (maphash #'(lambda (k v) (when (< v 10) (remhash k *h*))) *h*)
  ```
