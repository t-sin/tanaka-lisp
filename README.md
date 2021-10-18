# tanaka-lisp

An embeddable Lisp. Dedicated with [him](https://oddtaxi.fandom.com/wiki/Hajime_Tanaka).

## features

- basic features
    - lexical bindings
    - without dynamic bindings
    - rest arguments
    - no optional, keyword arguments
- data types
    - integers
    - floating point numbers
    - strings
    - arrays
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
(dfn pulse (ph duty)
  (if (> ph duty)
      1.0
      0.0))
```

```lisp
;; I/O example
(with-open-file (in "README.md" :input :text)
  (read-line in))
; => "# tanaka-lisp" nil
```

```lisp
;; error handling example
(defun div (a b)
  (if (= b 0)
    (error "division by zero")
    (/ a b)))

(div 10 5)
; => 2

(div 10 0)
; => #<error "division by zero">

(try
    (div 10 0)
  ((arithmetic-error (_) (print "division by zero"))))
; => division by zero
```

```lisp
;; binding multiple values
(bind (quotient err)
    (div 10 5)
  (print quotient))
; => 2

;; destructuring binding
(bind ((car . cdr) ; destructuring pattern
       2nd)
    (values (list 1 2) #t)
  (print (list car cdr 2nd)))
; => (1 2 #t)

;; `bind` is a macro.
;; the multiple-value example is expanded as like this:
(multiple-value-call
  (lambda (quotient err)
    (print quotient))
  (div 10 5))
```

```lisp
;; object oriented programming example

;; creating classes
;; `#{}` is a hash table literal
;; class is a hash table that has a hash table named as `*super*`
(setq *parent-object*
  #{:*super* #{}
    :method (lambda (self) (print "Ku!"))})
; => #{...}

(setq *child-object*
  #{:*super* *parent-object*
    :method (lambda (self)
                    (send :method (gethash :*super* self))
                    (print "Kyu!!"))})
; => #{...}

(send :method *parent-object*)
; => Ku!

(send :method *child-object*)
; => Ku!
;    Kyu!!
```
