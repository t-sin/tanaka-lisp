# tanaka-lisp

It's a Lisp for [Hajime Tanaka](https://oddtaxi.fandom.com/wiki/Hajime_Tanaka).

## features

- data types
    - integers
    - strings
    - arrays
    - structures
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
- mini compiler?

## examples

```lisp
(print "hello world!")
```

```lisp
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
    (return nil "division by zero")
    (return (/ a b) nil)))

(div 10 5)
; => 2 nil

(div 10 0)
; => nil "division by zero"
```

```lisp
(block open-file
  (with-open-file (in "filename"
                   :direction :input
                   :error err)
    (when (null err)
      (return-from open-file))
    (read-line in)))
```
