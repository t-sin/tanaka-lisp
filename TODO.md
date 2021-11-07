# tanaka-lisp implementation progress


## basic features

- [ ] UTF-8 encoder/decoder
- [ ] I/O streams
    - [ ] binary streams
    - [ ] text streams

## layer1 (Lisp layer)

- [ ] basic data types in REPL
    - [ ] boolean values `#t` and `#f`
    - [ ] integers
    - [ ] floating point numbers
    - [ ] dot pairs
        - [ ] garbage collection
    - [ ] proper lists
    - [ ] arrays
    - [ ] hash tables
    - [ ] symbols
    - [ ] keywords
- [ ] special forms in REPL
    - [ ] `and`
    - [ ] `or`
    - [ ] `cond`
    - [ ] `if`
    - [ ] `loop`
    - [ ] `let`
    - [ ] `lambda`
    - [ ] `defun`
    - [ ] `bind`
    - [ ] `values`
    - [ ] `set`
        - [ ] for symbols
        - [ ] arrays
        - [ ] hash tables
    - [ ] `try` and `error`
- [ ] equality
    - [ ] `eq` ... object equality
    - [ ] `=` ... number equality
    - [ ] `equal` ... shallow equality; `(or (= a b) (eq a b))`
- [ ] type predicates `xxx?`
    - [ ] `boolean?`
    - [ ] `integer?`
    - [ ] `float?`
    - [ ] `cons?`
    - [ ] `list?`
    - [ ] `array?`
    - [ ] `hash-table?`
    - [ ] `symbol?`
    - [ ] `keywords?`
    - [ ] `function?`
    - [ ] `type-of`
- [ ] data manipulation functions
    - [ ] list manipulations
    - [ ] array manipulations
    - [ ] string manipulations
    - [ ] hash table manipulations
- [ ] `eval` function
- [ ] file loading facility
- [ ] preparation for layer2
    - [ ] `make-object` function
    - [ ] `object?`
    - [ ] `find-message` function
    - [ ] `send` function
    - [ ] `define-message` function

## layer2 (object-oriented layer)

- [ ] define objects
    - [ ] root object
        - [ ] `name` message
        - [ ] `parent` message
        - [ ] `to-lisp-string` message
        - [ ] `to-string` message
        - [ ] `print` message
        - [ ] `descendant?` message
        - [ ] `eval` message
    - [ ] boolean
        - [ ] `#true`
        - [ ] `#false`
    - [ ] integer
        - [ ] `construct`
        - [ ] `to-lisp-string`
        - [ ] `+`
    - [ ] strings
    - [ ] symbol
- [ ] stack objects; to implement continuations
- [ ] object-oriented `read`
    - [ ] I/O streams
- [ ] object-oriented `eval`
    - [ ] `proc` objects
    - [ ] `runtime` objects

## dodo graphics

- [ ] PGM reader/writer
- [ ] graphics object
    - [ ] `construct`
    - [ ] `get`
    - [ ] `draw-pixel`
    - [ ] `draw-line` with Bresenham's line algorithm
- [ ] dodo graphics
