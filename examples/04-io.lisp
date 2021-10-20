;; I/O example
(with-open-file (in "README.md" :input :text)
  (read-line in))
; => "# tanaka-lisp" nil
