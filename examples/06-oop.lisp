;; object oriented programming example

;; creating classes
;; `#{}` is a hash table literal
;; class is a hash table that has a hash table named as `*meta*`
(setq *parent-object*
  #{:*meta* #{:name :parent :parent nil}
    :method (lambda (self) (println "Ku!"))})
; => #{...}

(setq *child-object*
  #{:*meta* #{:name :child :parent *parent-object*}
    :method (lambda (self)
              (send :method (get (get self :*meta*) :parent))
              (println "Kyu!!"))})
; => #{...}

(send :method *parent-object*)
; => Ku!

(send :method *child-object*)
; => Ku!
;    Kyu!!
