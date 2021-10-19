;; object oriented programming example

;; creating classes
;; `#{}` is a hash table literal
;; class is a hash table that has a hash table named as `*meta*`
(setq *animal*
  #{:*meta* #{:name :animal :parent nil}
    :say (lambda (self) (println "Animal!"))})
; => #{...}

(setq *dodo*
  #{:*meta* #{:name :dodo :parent *animal*}
    :say (lambda (self)
              (send :say (get (get self :*meta*) :parent))
              (println "Dodo!!"))})
; => #{...}

(send :say *animal*)
; => Animal!

(send :say *dodo*)
; => Animal!
;    Dodo!!
