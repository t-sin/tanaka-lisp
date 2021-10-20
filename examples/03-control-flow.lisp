;; conditional branching and loop examples

;; cond
(let ((n 0))
  (cond ((= n 0) (println "0!!\n"))
        ((= n 1) (println "1!!\n"))
        (t (println "others!!\n"))))

;; loop
(let ((n 0))
  (loop
    :while (< n 3)  ; conditional form (optional)
    :do (progn      ; body (implicit progn)
          (println "n = {}\n" n)
          (set n (1+ n)))))
;    0
;    1
;    2
; => 2 (return value of `(set n ...)`)
