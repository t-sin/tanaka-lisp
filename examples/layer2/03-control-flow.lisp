;; cotrol flow examples in OOP

;; conditional branching
(write (stdout)
       (cond ((= n 10) (format "10!!\n"))
             ((= n 20) (format "20!!\n"))
             (#true (format "others!\n"))))

(write (stdout)
       (if (> n 0)
           (format "positive\n")
           (format "negative\n")))

;; loop
;; レキシカル束縛ってOOPでは何…？
(let #{n 0}
  (loop
    :while (< n 3)
    (do ; `progn` in Common Lisp
        (write (stdout) (format "n = {}\n" n))
        (set n (1+ n)))))

;; `let`は糖衣構文
;; `実行されないコードオブジェクト`proc` (レキシカルクロージャ) を用いて、以下の例
(let #{n 0} ...)
;; はこうなる
(send (proc (n) ...) 0)
