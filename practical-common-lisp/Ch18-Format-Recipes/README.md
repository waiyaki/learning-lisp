# The `FORMAT` function
- Takes 2 required arguments - a destination for the output and a control string
  that contains literal text and embedded directives.
- Any additional arguments are used by the directives in the control string to
  interpolate values into the output.

- First argument to `format` can either be `T`, `NIL`, a stream or a string with a fill pointer.
  - `T` - Shorthand for `*standard-output*`
  - `NIL` - Causes format to generate output to a string, which it then returns.
  - stream - Output is written to the stream.
  - string with fill pointer - Formatted output is added to the end of the string and fill
    pointer is adjusted appropriately.


## `FORMAT` Directives
- Start with a tilde (`~`) and end with a single character that identified the directive.
- Some directives take prefix parameters, written immediately following the tilde, separated
  by commas.
  ```lisp
  CL-USER> (format t "~$" pi)
  3.14
  NIL

  ;; With prefix parameters
  CL-USER> (format t "~5$" pi)
  3.14159
  NIL
  CL-USER>
  ```

- Prefix parameters can either be decimal numbers or characters.
- Value of prefix parameter can also be derived from the format arguments in two ways:
  - `v` causes `format` to consume one format argument and use it as its value for prefix param.
    ```lisp
    CL-USER> (format t "~v$" 3 pi)
    3.142
    NIL
    ```
  - `#` will be evaluated as the number of remaining arguments to format.
    ```lisp
    CL-USER> (format t "~#$" pi)
    3.1
    NIL
    CL-USER>
    ```

- Can use a comma to skip over unspecified parameters, e.g., `~f` takes a parameter to specify the
  number of decimal places to print to, as it's second parameter.
  ```lisp
  CL-USER> (format t "~,5f" pi) ;; Use `,` to skip over first argument to `~f`.
  3.14159
  NIL
  CL-USER>
  ```


- Can modify behaviour with `colon` and `at-sign` _modifiers_.
- Placed after any prefix parameters and before the directive's identifying character.
- Change directive behaviour in small ways, e.g., for `~D`, the directive for integer output:
```lisp
CL-USER> (format t "~d" 1000000)
1000000
NIL
CL-USER> (format t "~:d" 1000000)
1,000,000
NIL
CL-USER> (format t "~@d" 1000000)
+1000000
NIL
CL-USER> (format t "~:@d" 1000000)
+1,000,000
NIL
CL-USER>
```

## Basic Formatting
- `~A` - Consumes one format argument of any type and outputs it in an _aesthetic_ (human readable)
  form.
  ```lisp
  CL-USER> (format nil "The value is ~a" 10)
  "The value is 10"
  CL-USER> (format nil "The value is ~a" "foo")
  "The value is foo"
  CL-USER> (format nil "The value is ~a" (list 1 2 3))
  "The value is (1 2 3)"
  CL-USER>
  ```
- `~S` - Consumes one format argument and tries to generate output that can be read back in using
  `READ`.
  ```lisp
  CL-USER> (format nil "The value is ~s" 10)
  "The value is 10"
  CL-USER> (format nil "The value is ~s" "foo")
  "The value is \"foo\""
  CL-USER> (format nil "The value is ~s" (list 1 2 3))
  "The value is (1 2 3)"
  CL-USER>
  ```
  Objects without a readable format are printed using the unreadable syntax `#<>`.
  With a colon modifier, both `~A` and `~S` output `NIL` as `()`.

- `~%` - Emits a newline. Always.
- `~&` - Emits a newline. Only if it's not already at the beginning of a line.
- `~~` - Emits a literal tilde.

## Character and Integer Directives.
- `~C` - Used to emit characters. With a colon modifier, can emit non-printing characters such
  as `#\NEWLINE` or `#\SPACE` by name. With the at-sign modifier, `~@C` will emit characters in the
  Lisp syntax.
- `~D, ~X, ~O, ~B` - Integers base 10, base 16, base 8 and base 2 respectively.
- `~R` - General radix directive, first parameter a number between 2 - 36, which is the base to use
  - Has special behaviour when used with no params.

## Floating-Point Directives
- `~F` - Emits it's (number) argument in decimal format, possibly controlling number of decimal
  places after the decimal point.
- `~E` - Emits always emits number in computerised scientific notation.
- `~$` - Similar to `~F` but simpler. Meant for monetary output.

## English Language Directives.
- `~R` - With no params, emits numerical input as English words or roman numerals.
  - Without prefix or modifier, emits numbers in words as a cardinal number.
  ```lisp
  CL-USER> (format nil "~r" 1234)
  "one thousand two hundred thirty-four"
  CL-USER>
  ```
  - With modifier, emits number in ordinal
  ```lisp
  CL-USER> (format nil "~:r" 1234)
  "one thousand two hundred thirty-fourth"
  CL-USER>
  ```
  - With `@` modifier, emits roman numerals.
  - With both `:` and `@` modifiers, emits old style roman numerals, where fours and nines are written as
    `IIII` and `VIIII` instead of `IV` and `IX`.
  ```lisp
  CL-USER> (format nil "~@r" 1234)
  "MCCXXXIV"
  CL-USER> (format nil "~:@r" 1234)
  "MCCXXXIIII"
  CL-USER>
  ```
  - Behaves like `~D` for large numbers.
- `~P` - Helps generate messages with words properly pluralized.
  - With `:` modifier, reprocesses previous format argument.
  - With `@` modifier, emits either _y_ or _ies_.

- `~(` paired with `~)` - Converts text to lowercase.
  - With `@`, will capitalize the first word in a section of text.
  - With `:`, will capitalize all words in a section of text.
