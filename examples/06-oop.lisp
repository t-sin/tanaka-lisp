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


;; Smalltalk's `doesNotUnderstand`
;; or Ruby's `method_missing`
(setq obj #{:*meta* #{:parent nil}
            :unknown-message (lambda (self msg &rest args)
                               (cl:format t "unknown message ~s with args: ~s" msg args))})

(send :hoge obj 1 2)
; => unknown message :hoge with args (1 2)

;; utilities
(make-object child *parent-object*)
; => #{:*meta* #{:name :child :parent *parent-object*}}

(define-message say obj (name)
  (println "say {}!" name))
