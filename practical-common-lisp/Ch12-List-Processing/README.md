# List Processing
- Lists in Lisp are an illusion built atop objects that are instances of a more primitive data type, which are a pair of values called *_cons cells_*.
- Created using `cons` function.
- `cons` takes 2 arguments, and returns a new cons cell with two values. The values can reference any type of object.

- A cons is printed as two values in parens separated by a dot (a dotted pair) unless the second value is `nil` or another cons cell.
  ```lisp
  CL-USER> (cons 1 2)
  (1 . 2)
  CL-USER>
  ```

- The two values in the cons cell are `car` and `cdr` after the names of the functions used to access them.
  ```lisp
  CL-USER> (car (cons 1 2))
  1
  CL-USER> (cdr (cons 1 2))
  2
  CL-USER>
  ```

- `car` and `cdr` are `setf`able.
  ```lisp
  CL-USER> (defparameter *cons* (cons  1 2))
  *CONS*
  CL-USER> *cons*
  (1 . 2)
  CL-USER> (setf (car *cons*) 10)
  10
  CL-USER> *cons*
  (10 . 2)
  CL-USER> (setf (cdr *cons*) 20)
  20
  CL-USER> *cons*
  (10 . 20)
  CL-USER>
  ```

- Link cons cells together to build larger structures. Lists are built by linking cons cells in a chain. Elements of list are held in `car`s and the links to subsequent cons cells are held in `cdr`s. Last cons cell in the chain has a `cdr` of `nil`. This arrangement is a *_singly linked list_*
  ```lisp
  CL-USER> (cons 1 nil)
  (1)
  CL-USER> (cons 1 (cons 2 nil))
  (1 2)
  CL-USER> (cons 1 (cons 2 (cons 3 nil)))
  (1 2 3)
  CL-USER>
  ```

- The `list` function builds cons cells and links them.

## Functional Programming and Lists
_Explanation in this subtopic makes sense. Meaning I probably didn't totally understand this. Need to read it again sometime._

## List-Manipulation Functions
- `first` and `rest` - Get first, remaining items of a list.
- `second` to `tenth` - Get the corresponding item from a list.
- `nth` - Takes 2 args, an index and a list and returns the _nth_ (zero-based) element of the list.
- `nthcdr` - Takes an index and a list and returns the result of calling `cdr` _n_ times.

> None of these functions are any more efficient than the other in terms of work done by the computer to the equivalent calls to `first` and `rest` combos. There's no way to get to the _nth_ element without following _n_ `cdr` references.

- Composite `car`/`cdr` functions - 28 in number.
  ```
  (caar list) === (car (car list))
  (cadr list) === (car (cdr list))
  (cadadr list) === (car (cdr (car (cdr list))))
  ```

  Many of these functions only make sense when applied to lists that contain other lists.
  ```lisp
  (caar (list 1 2 3))                  ; => error
  (caar (list (list 1 2) 3))           ; => 1
  (cadr (list (list 1 2) (list 3 4)))  ; => (3 4)
  (caadr (list (list 1 2) (list 3 4))) ; => 3
  ```

| Function | Description |
| :------------- | :------------- |
| `LAST`      | Returns the last cons cell in a list. With an integer, argument returns the last n cons cells. |
| `BUTLAST`   | Returns a copy of the list, excluding the last cons cell. With an integer argument, excludes the last n cells. |
| `NBUTLAST`  | The recycling version of `BUTLAST`; may modify and return the argument list but has no reliable side effects. |
| `LDIFF`     | Returns a copy of a list up to a given cons cell. |
| `TAILP`     | Returns true if a given object is a cons cell that's part of the structure of a list. |
| `LIST*`     | Builds a list to hold all but the last of its arguments and then makes the last argument the `CDR` of the last cell in the list. In other words, a cross between `LIST` and `APPEND`. |
| `MAKE-LIST` | Builds an n item list. The initial elements of the list are `NIL` or the value specified with the :initial-element keyword argument. |
| `REVAPPEND` | Combination of `REVERSE` and `APPEND`; reverses first argument as with `REVERSE` and then appends the second argument. |
| `NRECONC`   | Recycling version of `REVAPPEND`; reverses first argument as if by `NREVERSE` and then appends the second argument. No reliable side effects. |
| `CONSP`     | Predicate to test whether an object is a cons cell. |
| `ATOM`      | Predicate to test whether an object is not a cons cell. |
| `LISTP`     | Predicate to test whether an object is either a cons cell or `NIL`. |
| `NULL`      | Predicate to test whether an object is `NIL`. Functionally equivalent to `NOT` but stylistically preferable when testing for an empty list as opposed to boolean false. |


## Mapping
- `mapcar` - Like `map`, but always returns a list and doesn't require the result-type argument `map` does. First argument is function to apply, rest arguments are lists whose elements will be arguments to the function.
  ```lisp
  CL-USER> (mapcar #'(lambda (x) (* x 2)) '(1 2 3 4))
  (2 4 6 8)
  CL-USER> (mapcar #'+ (list 1 2 3) (list 20 30 40))
  (21 32 43)
  CL-USER>
  ```

- `maplist` - Like `mapcar`, except instead of passing the elements of the list to the function, it passes the actual cons cells. As such, the function has access not only to the value of each element of the list (via the `CAR` of the cons cell) but also to the rest of the list (via the `CDR`).

- `mapcan` and `mapcon` - Work like `mapcar` and `maplist` except for how they build up their result. While `MAPCAR` and `MAPLIST` build a completely new list to hold the results of the function calls, `MAPCAN` and `MAPCON` build their result by splicing together the results, which must be lists, as if by `NCONC`. `MAPCAN`, like `MAPCAR`, passes the elements of the list to the mapped function while MAPCON, like `MAPLIST`, passes the cons cells.

- `mapc` and `mapl` - Simply return their first argument. `mapc` is a cousin of `mapchar`/`mapcan` while `mapl` is of the `maplist`/`mapcon` family.
