;; conditional branching and loop examples

;; cond
(let ((n 0))
  (cond ((= n 0) (format t "0!!\n"))
        ((= n 1) (format t "1!!\n"))
        (t (format t "others!!\n"))))

;; loop
(let ((n 0))
  (loop
    :while (< n 3)  ; conditional form (optional)
    :do (do         ; body
          (format t "n = {}\n" n)
          (set n (1+ n)))))
;    0
;    1
;    2
; => 2 (return value of `(set n ...)`)
