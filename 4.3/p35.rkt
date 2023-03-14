#lang sicp

(define (an-integer-between f c)
  (amb-require (<= f c))
  (amb f (an-integer-between (+ f 1) c)))
