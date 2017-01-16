# Ch-09 - Building a Unit Test Framework
```lisp
(defun v0-test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
```

Refactoring `v0-test-+` so that we return either `T` or `NIL` depending on whether the tests are failing or passing and also report on individual test cases. Use `report-result` to factor out duplicated formatting
```lisp
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))
```

Rewrite `test-+` using `report-result` in place of format.
```lisp
(defun v1-test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
```

Refactor `report-result` to remove the duplication by writing a macro `check` that will treat the test form as both code and data, such that you can write:
```lisp
(check (= (+ 1 2) 3))
```
and have it mean:
```lisp
(report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
```

```lisp
(defmacro check-v0 (form)
  `(report-result ,form ',form))

(defun v2-test-+ ()
  (check-v0 (= (+ 1 2) 3))
  (check-v0 (= (+ 1 2 3) 6))
  (check-v0 (= (+ -1 -3) -4)))
```

Refactor `check` so that we can get rid of the multiple calls to `check` in the client code
```
(defmacro check-v1 (&body forms)
  `(progn
     ,@(loop for form in forms collect `(report-result ,form ',form))))

(defun v3-test-+ ()
  (check-v1
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))
```

Refactor `test-+` to return a value indicating if all the tests passed or failed. Start by editing `report-result` so that it returns the result of the test case it's reporting
```
(defun report-result-v1 (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)
```

Construct a macro that will behave like `AND` without the short-circuiting. We need to write a macro, `combine-results` that we can use as:
```lisp
(combine-results
  (foo)
  (bar)
  (baz))
```
and have it mean:
```lisp
(let ((result t))
  (unless (foo) (setf result nil))
  (unless (bar) (setf result nil))
  (unless (baz) (setf result nil))
  result)
```

Pull in `with-gensyms` to make sure that the variable `result` introduced in the macro doesn't introduce a leak.
```lisp
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
          ,result)))
```
Fix `check` by making it use `combine-results` instead of `progn`
```lisp
(defmacro check-v2 (&body forms)
  `(combine-results
     ,@ (loop for f in forms collect `(report-result-v1 ,f ',f))))

(defun v4-test-+ ()
  (check-v2
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))
```
Refactor to make the report include the function the tests are running from. This will eventually aid in organizing similar tests into test suites.

Create a dynamic variable `*test-name*` that each test function binds to the name of the function before calling `check`. `report-result` can then use it.
```lisp
(defvar *test-name* nil)

(defun report-result-v2 (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check-v3 (&body forms)
  `(combine-results
     ,@ (loop for f in forms collect `(report-result-v2 ,f ',f))))

(defun v5-test-+ ()
  (let ((*test-name* 'v5-test-+))
    (check-v3
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6)
      (= (+ -1 -3) -4))))

(defun test-* ()
  (let ((*test-name* 'test-*))
     (check-v3
       (= (* 2 3) 6)
       (= (* 3 5) 15))))

(defun test-arithmetic ()
  (combine-results
    (test-*)
    (v5-test-+)))
```

Refactor to remove the duplication introduced when reporting, the function name is used twice, once in the `defun` and again while binding the dynamic variable. Also abstract the binding of the dynamic variable, whose pattern is duplicated. Perform this abstraction by writing a macro that will expand into a `defun`, then use this macro instead of `defun` to define test functions.
```lisp
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* ',name))
       ,@body)))
```
Rewrite `test-+` using `deftest`
```lisp
(deftest v5-test-+ ()
  (check-v3
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))
```
Refactor `deftest` to maintain a "fully qualified" path to all function calls
that will enable organization of tests into test suites.
```lisp
(defmacro deftest-v2 (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-arithmetic-v2 ()
  (combine-results
    (test-*)
    (v5-test-+)))
```
