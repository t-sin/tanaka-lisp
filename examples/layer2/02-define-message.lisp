;; message definition example

(defmsg pulse (type float)
    (duty)
  (if (> self duty)
      1.0
      0.0))

(pulse 0.0 0.5)
; => 0.0

(pulse 0.8 0.5)
; => 1.0

(pulse 1 1)
; -> unknown message (because `1` is integer)
