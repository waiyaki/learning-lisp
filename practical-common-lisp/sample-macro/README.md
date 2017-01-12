## Macro parameters
 Macro parameters are 'destructuring' parameter lists, meaning you don't have
 to take them apart by hand.
 Can destructure by replacing the single parameter with a nested list parameter
 list. The parameters in the nested list will take their values from the elements
 that would have been bound to the parameter list replaced.
 We can replace `var-and-range` with `(var start end)`.
 We can also use &body instead of &rest when defining macros. Semantically equivalent,
 but editors indent uses of the macro with &body differently from &rest.
 Destructuring parameter lists also give you automatic error checking since Lisp is
 able to detect a call whose first argument isn't a 3 element list.
 Destructuring parameter lists can also contain &optional, &key and &rest params.

## Quoting
A backquoted expression is similar to a quoted expression except you can "unqoute"
 particular subexpressions by preceding them with a comma, possibly followed by an @ sign
 Without the @, the comma causes the value of the subexpression to be included as is.
 With the @, the value of the subexpression - which must be a list - is spliced into the
 enclosing list.

## Expanding macros

- Can use **`MACROEXPAND-1`** which takes any Lisp expression and returns the result of doing one level of macro expansion. To pass it a literal macro form, you must quote the form, e.g.
```lisp
(macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))
```
- In SLIME, can use `C-c RET` to invoke `slime-macroexpand-1` that'll do the same thing and print the result in a temporary buffer.

## Plugging macro leaks.
- _leaky abstraction_ - An abstraction that leaks details it's supposed to be abstracting away. Taking f.e the following snippet:
```lisp
(defmacro do-primes-v2 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))
```
The end form in evaluated too many times. Suppose instead of calling it as `(do-primes-v2 (p 0 19) (...))` we call it as `(do-primes-v2 (p 0 (random 100)) (...))`. When this is executed, the `RANDOM` will be called every time the end form is executed. Instead of looping till `p` is greater than the originally chosen number, it'll loop till it happens to draw a random number less than or equal to the current value of `p`.
This leaks abstraction since for the user to use `do-primes` correctly, they need to be aware that the end form will be evaluated multiple times. Plugging this leak entails generating code that evaluates `end` once and saves the resulting value for later use.
```lisp
(defmacro do-primes-v3 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end))
       ((> ,var ending-value))
     ,@body))
```


The macro `do-primes-v3` introduces a new leak by using the variable name `ending-value` since that variable name will clash if the parameter `var` has the name `ending-value`, i.e. if we call it as `(do-primes-v3 (ending-value 0 19) (...))`. Some Lisps will reject this code, or it loop forever since `ending-value` will never be greater than itself.

To plug this leak, we'll need a symbol that's never used outside the code generated my the macro. The function `GENSYM` returns a new symbol each time it's called. Instead of a literal name like `ending-value`, we can generate a new symbol each time `do-primes` is expanded.
```lisp
(defmacro do-primes-v4 ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))
```

Now, the call
```Lisp
(do-primes-v4 (ending-value 0 19)
  (print ending-value))
```
expands to
```Lisp
(DO ((ENDING-VALUE (NEXT-PRIME 0) (NEXT-PRIME (1+ ENDING-VALUE)))
     (#:G849 19))
    ((> ENDING-VALUE #:G849))
  (PRINT ENDING-VALUE))
```

The code that calls `gensym` isn't part of the expansion, it runs as part of the macro expander hence creates a new symbol each time the macro is expanded. Gensymed symbols are printed in the normal syntax for uninterned symbols, with a leading `#:`.

### Rules for writing preplugged macros:
- Unless there's a particular reason to do otherwise, include any subforms in the expansion in  positions that will be evaluated in the same order as the subforms appear in the macro call.
- Unless there's a particular reason to do otherwise, make sure subforms are evaluated only once by creating a variable in the expansion to hold the value of evaluating the argument form and then using that variable anywhere else the value is needed in the expansion.
- Use `GENSYM` at macro expansion time to create variable names used in the expansion.

## Macro writing Macros
The job of macros is to abstract away common syntactic patterns, and certain patterns will appear again and again while writing macros. For example, many macros will start with a `LET` that introduces a few variables holding gensymed symbols to be used in the macro's expansion. This pattern can be abstracted away in its own macros (macros that generates macros - code that generates code that generates code)

```lisp
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes-v5 ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))
```
