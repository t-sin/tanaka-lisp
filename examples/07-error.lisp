;; error handling example

(set error (make-object :error nil))
(set arithmetic-error (make-object :arithmetic-error error))

(defun div (a b)
  (if (= b 0)
    (error arithmetic-error)
    (/ a b)))

(div 10 5)
; => 2

(try
    (div 10 0)
  (arithmetic-error (_) (format t "{}" "division by zero\n")))
; => division by zero

(div 10 0)
; => #<error "division by zero">
