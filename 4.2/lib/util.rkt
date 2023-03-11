#lang sicp


(define (tagged-list? expr tag)
  (if (pair? expr)
    (eq? (car expr) tag)
    #f))

(#%require (only racket provide))

(provide
  tagged-list?)
