;; function definition example
;; square wave generator
(defun pulse (ph duty)
  (if (> ph duty)
      1.0
      0.0))

(pulse 0.0 0.5)
; => 0.0

(pulse 1.0 0.5)
; => 1.0
