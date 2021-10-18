;; object oriented programming example

;; creating classes
;; `#{}` is a hash table literal
;; class is a hash table that has a hash table named as `*meta*`
(setq *parent-object*
  #{:*meta* #{}
    :method (lambda (self) (print "Ku!"))})
; => #{...}

(setq *child-object*
  #{:*meta* *parent-object*
    :method (lambda (self)
                    (send :method (gethash :*meta* self))
                    (print "Kyu!!"))})
; => #{...}

(send :method *parent-object*)
; => Ku!

(send :method *child-object*)
; => Ku!
;    Kyu!!
