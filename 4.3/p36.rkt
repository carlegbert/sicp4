#lang sicp

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ 1 n))))

(define (arbitrary-pythag-triple)
  (let* ((flr (an-integer-starting-from 0))
         (ceil (an-integer-starting-from flr))
         (mid (an-integer-between flr ceil)))
    (amb-require (= (+ (square flr) (square mid))
                    (square ceil)))
    (list flr ceil mid)))
