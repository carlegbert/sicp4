#lang racket

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
       #t
       (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
       #f
       (ev? ev? od? (- n 1))))))
