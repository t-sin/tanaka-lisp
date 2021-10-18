;; error handling example
(defun div (a b)
  (if (= b 0)
    (error "division by zero")
    (/ a b)))

(div 10 5)
; => 2

(try
    (div 10 0)
  (arithmetic-error (_) (println "{}" "division by zero")))
; => division by zero

(div 10 0)
; => #<error "division by zero">
