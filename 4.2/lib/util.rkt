#lang racket

(require rnrs/mutable-pairs-6)

(define (tagged-list? expr tag)
  (cond ((mpair? expr)
         (eq? (mcar expr) tag))
        ((pair? expr)
         (eq? (car expr) tag))
        (else #f)))

(define (make-mutable item)
  (if (pair? item)
    (mcons (make-mutable (car item))
           (make-mutable (cdr item)))
    item))

(provide
  tagged-list?
  make-mutable)

