;; I/O examples in OOP

(try
    ; (open FILENAME IS_INPUT IS_TEXT)
    (let ((in (open "README.md" #true #true)))
      (read-line in))
  (proc (e) ; code object like `lambda` in Common Lisp
    (write (stdout)
           (format "error: {}\n" (to-string e))
           (return))))
; => "# tanaka-lisp"
