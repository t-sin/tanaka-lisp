;; binding multiple values
(bind (quotient)
    (div 10 5)
  (println "{}" quotient))
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
