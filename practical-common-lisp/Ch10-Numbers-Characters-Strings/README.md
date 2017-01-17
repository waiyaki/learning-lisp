# Numbers, Characters and Strings

## Recap
- **Lisp Reader** - Responsible for translating text into Lisp objects.
- **Lisp Evaluator** - Deals with objects translated by the reader.
  For a given type, there can be many textual representations, all converted to the same object representation by the Lisp reader.

## Numbers

### Syntax
- Syntax for integers is an optional sign (+, -) followed by one or more digits.
- Syntax for ratios is an optional sign, a sequence of digits followed by a slash followed by another sequence of digit.
```lisp
1/3
4/5
123/98
```
​    Rationals are printed in "reduced" form, i.e. `20/10` gets reduced to `2` and `2/6` to `1/3`.
- Syntax for floating point numbers is an optional sign followed by a nonempty sequence of digits, possibly with an embedded decimal point. Can also be followed by the exponent marker for "computerized scientific notation"
```lisp
1.23
1e10
123e-3
```

- Complex numbers are written as `#C` or `#c` followed by a list of two real numbers representing the real and imaginary parts of a complex number.
```lisp
#c(2 1)    ; => #c(2 1)
#c(1/2 1)  ; => #c(0.5 1)
#c(-6/3 0) ; => -2
```

### Basic Math
- If all arguments are of the same type, the result will be of that type except if a complex number operation yields a number with a zero imaginary component, in which case the result will be rational.
- If the arguments are a mixture of real and floating-point numbers, the arguments are converted to the nearest floating point value.
- If any arguments are complex, the rest of the arguments are converted to equivalent complex values.

Because `/` doesn't truncate, Common Lisp provides utilities for converting real numbers to an integer.
- `floor` truncates towards -infinity.
- `ceiling` truncates towards +infinity.
- `truncate` truncates towards zero (equivalent to `floor` for positive args and `ceiling` for negative args)
- `round` rounds off to the nearest integer. If the argument is exactly halfway between two integers, it rounds to the nearest even integer.


- `mod` - gets the modulus
- `rem` - get the remainder of a truncating division on real numbers.

The functions `1+` and `1-` provide a shorthand way of expressing adding and subtracting one from a number. Different from the macros `incf` and `decf` in that `1+` and `1-` are functions that return new values while `incf` and `decf` modify a place.
```Lisp
(incf x) == (setf x (1+ x)) == (setf x (+ x 1))
(decf x) == (setf x (1- x)) == (setf x (- x 1))
(incf x 10) == (setf x (+ x 10))
(decf x 10) == (setf x (- x 10))
```

### Numerical Comparisons
- The function `=` is the numeric equality predicate. Compares numbers by mathematical value, ignoring differences in types. `=` would consider numbers equivalent where `eql` would consider them inequivalent due to type differences.
- The generic equality predicate `equalp` uses `=` to compare numbers.

- The `/=` function returns true only if all it's arguments are different values.
```Lisp
(/= 1 1)        ; ==> NIL
(/= 1 2)        ; ==> T
(/= 1 2 3)      ; ==> T
(/= 1 2 3 1)    ; ==> NIL
(/= 1 2 3 1.0)  ; ==> NIL
```

The functions `<`, `>`, `<=`, `>=` order rationals and floating-point numbers (real numbers). When called with more than two args, each arg is compared to the arg to its right.

`min` and `max` - used to pick smallest and largest numbers in a list respectively.

`zerop` - test whether a single real number is equal to zero.
`minusp` - test whether a single real number is less than zero.
`plusp` - test whetheer a single real number is greater than zero.
`evenp` - test if even number.
`oddp` - test if odd number.

> The `p` suffix in the functions is a naming convention for predicate functions. (functions that test a condition and return a boolean).

### Higher Math
- `log` - logarithms
- `exp` and `expt` - exponentiation
- `sin`, `cos`, `tan`, `asin`, `acos`, `atan` - basic trig functions.
- `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh` - hyperbolic functions.

Also provides functions to get the individual bits of an integer and extract the parts of a ratio or a complex number.

## Characters
Common Lisp characters are a distinct type of object from numbers.

### Syntax
- Read syntax for character objects is `#/` followed by the desired character, i.e. `#/x` is the character `x`.
- Alternative syntax for some characters is `#/` followed by the character name, e.g `#/SPACE` for whitespace. Other such characters are `Tab`, `Page`, `Newline`, `Linefeed`, `Return`, `Backspace`.

### Character Comparisons
- `char=` - Case-sensitive character comparison. Can take any number of characters and return `T` if they're all the same character. Case-insensitive equivalent is `char-equal`.
- Other equivalents to the numeric counterparts are:


| Numeric Analog | Case-Sensitive	| Case-Insensitive  |
| :------------: | :------------:   | :---------------: |
| =	             | CHAR=	        | CHAR-EQUAL        |
| /=	         | CHAR/=	        | CHAR-NOT-EQUAL    |
| <	             | CHAR<	        | CHAR-LESSP        |
| >	             | CHAR>	        | CHAR-GREATERP     |
| <=	         | CHAR<=	        | CHAR-NOT-GREATERP |
| >=	         | CHAR>=	        | CHAR-NOT-LESSP    |

## Strings
A composite data type - a one dimensional array of characters.

### String Comparisons
- Can use functions named similarly as the character comparison functions, except with `string` in place of `char`, e.g `string=`, `string-not-greaterp`.
- Unlike character or number comparators, string comparators can only compare two strings because they also support keyword args that allow you to compare substrings.

The arguments `:start1`, `:end1`, `:start2`, and `:end2` specify the starting (inclusive) and ending (exclusive) indices of substrings in the first and second string arguments. Thus, the following:
```lisp
(string= "foobarbaz" "quuxbarfoo" :start1 3 :end1 6 :start2 4 :end2 7)
```
compares the substring "bar" in the two arguments and returns true. The `:end1` and `:end2` arguments can be `NIL` (or the keyword argument omitted altogether) to indicate that the corresponding substring extends to the end of the string.

- Can discover length of a string using `length`.
- Can set individual characters using `elt` or the generic array accessor `aref` or the string-specific accessor `char`.
