# Classes

To define classes that can be instantiated with the object slots already created:
- Add an `:initarg` option to the slots. The `:initarg` name can be used as a keyword
parameter to `make-instance`.
- Add an `:initform`. Lets you specify a Lisp expression that'll be used to compute
a value for the slot if no `:initarg` argument is passed to `make-instance`.
- Define a method on the generic function `initialize-instance` called by `make-instance`.


Example: Allow makers of bank-account instances to pass customer name and initial balance.
Default to zero for the balance.
```lisp
(defclass bank-account-1 ()
  ((customer-name
    :initarg :customer-name)
   (balance
    :initarg :balance
    :initform 0)))

(defparameter *account*
  (make-instance 'bank-account-1 :customer-name "John Doe" :balance 1000))

(print (slot-value *account* 'customer-name)) ==> "John Doe"
(print (slot-value *account* 'balance)) ==> 1000
```

## Object Initialization

Can have a value that autoincrements for every instance creation.
```lisp
(defvar *account-numbers* 0)
```

Can ensure a parameter is provided when the object instance is created
```lisp
(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name"))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))
   account-type))
```

For complete control over the initialization of instances, define a method on
the generic function `initialize-instance`.
You can use this method to initialize the value of slots based on the
initialization values of other slots.
`initialize-instance`, by default, takes care of initializing slots based on their
`:initarg` and `:initform` options.
Most common way to add custom initialization code is to define an `:after` method
specialized on your custom class.

Suppose we need a class with `account-type` set to either `silver`, `gold` or `bronze`
based on the account balance. We specialize `initialize-instance` on the `bank-account` class.
Adding an `:after` to `initialize-instance` in Common Lisp's analog to defining a constructor
in Java or an `__init__` method in Python

The `&key` parameter is required, even if the method doesn't define it's own `&key` params,
to keep the method's parameter list congruent with the generic function's.
```lisp
(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
          (cond
            ((>= balance 100000) :gold)
            ((>= balance 50000) :silver)
            (t :bronze)))))
```

If an `initialize-instance` specialised on a class specifies and `&key` param, that param
becomes a legal param to `make-instance` when creating an instance of that class.

Suppose the bank pays a percentage of the initial balance as a bonus:
```lisp
(defmethod initialize-instance :after ((account bank-account)
                                       &key opening-bonus-percentage)
  (when opening-bonus-percentage
    (incf (slot-value account 'balance)
          (* (slot-value account 'balance) (/ opening-bonus-percentage 100)))))

(defparameter *acct* (make-instance
                      'bank-account
                      :customer-name "Sally Sue"
                      :balance 10000
                      :opening-bonus-percentage 5))
```

## Accessor Functions

Can define accessor functions to access slot values instead of using `slot-value`.
If you're going to subclass a class, define a generic accessor so it's easily extensible
to apply to the subclasses as well.
```lisp
(defgeneric balance (account))

(defmethod balance ((account bank-account))
  (slot-value account 'balance))

(defgeneric customer-name (account))

(defmethod customer-name ((account bank-account))
  (slot-value account 'customer-name))
```

Can provide a function to set values into slots by extending `setf`, providing a new place
it knows how to set.
A `setf` function can take any number of arguments, but the first argument is always the value
to be assigned to the place.

Could define a `setf` function to set `customer-name` like:
```lisp
(defun (setf customer-name) (name account)
(setf (slot-value account 'customer-name) name))

(setf (customer-name *acct*) "Sally Suzie")
(print (customer-name *acct*)) ==> "Sally Suzie"
```

Can make the `setf` function generic, like so:
```lisp
(defgeneric (setf customer-name) (value account))

(defmethod (setf customer-name) (value (account bank-account))
  (setf (slot-value account 'customer-name) value))

(setf (customer-name *acct*) "Sally Suzie") ==> "Sally Suzie"
```

It's not very Lispy to define accessor functions by hand.
`defclass` supports 3 slot options to allow automatic creation of reader
and writer functions for a specific slot.
`:reader` option specifies a name to be used as the name of the generic function.
When `defclass` is evaluated, the generic function is created, if doesn't already exist.
Then a method specializing its single argument on the new class is created and returning
the value of the slot is added to the generic function.

This means we can change the slot definition of `balance` in `bank-account` to:
```lisp
(balance
:initarg :balance
:initform 0
:reader balance)
```

`:writer` option is used to create a generic function and method for setting the value
of a slot.
Function and method created follow the requirements of a `setf` function, e.g
```lisp
(customer-name
:initarg :customer-name
:initform (error "Must supply a customer name")
:reader customer-name
:writer (setf customer-name))
```

Since you'll mostly want both reader and writer functions, `defclass` provides an
`accessor` option that defines both reader and corresponding `setf` function.
This means we'd typically write this:
```lisp
(customer-name
:initarg :customer-name
:initform (error "Must supply a customer name")
:accessor customer-name)
```

Putting these together, definition of `bank-account` would be:
```lisp
(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name")
    :accessor customer-name
    :documentation "Customer's name")
   (balance
    :initarg :balance
    :initform 0
    :reader balance
    :documentation "Current account balance")
   (account-number
    :initform (incf *account-numbers*)
    :reader account-number
    :documentation "Account number, unique within a bank")
   (account-type
    :reader account-type
    :documentation "Type of account, one of :gold, :silver or :bronze")))
```

## `WITH-SLOTS` and `WITH-ACCESSORS`.
- Both macros create a block of code in which simple variable names can be used
  to refer to slots on a particular object.
- `with-slots` provides direct access to slots, as if by `slot-value`.
- `with-accessors` provides a shorthand for accessor methods.


## Class-Allocated Slots
- `:allocation` slot option - Value can either be `:instance` or `:class` and defaults
   to `:instance`.
- With `:class` option, the slot has only a single value, stored in class
  and shared by all instances.
- Accessed the same as `:instance` slots, with `slot-value`
- Not similar to Python's _class_ fields (or Java's _static_ fields) since you can't
  access it's value without an instance.
- Used to save space when you have many instances accessing a reference to the same object.
