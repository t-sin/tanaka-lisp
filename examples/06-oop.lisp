;; object oriented programming example

;; creating classes
;; `#{}` is a hash table literal
;; class is a hash table that has a hash table named as `*meta*`
(setq *animal*
  #{:*meta* #{:name :animal :parent nil}
    :say (lambda (self) (format t "Animal!\n"))})
; => #{...}

(setq *dodo*
  #{:*meta* #{:name :dodo :parent *animal*}
    :say (lambda (self)
              (send :say (get (get self :*meta*) :parent))
              (format t "Dodo!!\n"))})
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
(make-object :dodo *parent-object*)
; => #{:*meta* #{:name :dodo :parent *parent-object*}}

(define-message say dodo ()
  (format t "say dodo!\n"))

(send :say dodo)
; => say dodo!
