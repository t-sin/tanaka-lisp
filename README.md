# tanaka-lisp

An embeddable Lisp. Dedicated with [him](https://oddtaxi.fandom.com/wiki/Hajime_Tanaka).

## features

- data types
    - integers
    - floating point numbers
    - strings
    - arrays
    - structures
    - hash table
- object oriented programming
    - meta hash table
    - Lua style
- control flow
    - tagbody/go
    - block/return
- macro system
- math functions
    - for basic sound processing
- I/O streams
    - binary streams
    - character streams
- multiple values
- Go style error system

## goals

- real-time sound generation with `play` command
- describing danmaku system

## examples

```lisp
;; hello world example
(print "hello world!")
```

```lisp
;; function definition example
;; square wave generator
(defun pulse (ph duty)
  (if (> ph duty)
      1
      0))
```

```lisp
;; error handling example
(defun div (a b)
  (if (= b 0)
    (values 0 "division by zero")
    (values (/ a b) nil)))

(div 10 5)
; => 2 nil

(div 10 0)
; => nil "division by zero"
```

```lisp
;; binding multiple values
(bind (quotient err)
    (div 10 5)
  (print quotient))
; => 2

;; `bind` is a macro.
;; I want it to cover a feature of `destructuring-bind` macro, or `match` macro?
;; multiple-value example is expanded as like this:
(multiple-value-call
  (lambda (&optional quotient err &rest _)
    (print quotient))
  (div 10 5))
```

```lisp
;; I/O example
(block open-file
  (with-open-file (in "README.md"
                   :direction :input
                   :type :text
                   :error err)
    (when (null err)
      (return-from open-file))
    (read-line in)))
; => "tanaka-lisp" nil
```

```lisp
;; object oriented programming example

;; creating classes
;; `#{}` is a hash table literal
;; class is a hash table that has a hash table named as `*super*`
(setq *parent-object*
  #{:*super* #{}
    :method-parent (lambda (self) (print "Ku!"))})
; => #{...}

(setq *child-object*
  #{:*super* *parent-object*
    :method-child (lambda (self)
                    (method-parent self)
                    (print "Kyu!!"))})
; => #{...}

(send :method-parent *parent-object*)
; => Ku!

(send :method-parent *child-object*)
; => Ku!
;    Kyu!!
```
