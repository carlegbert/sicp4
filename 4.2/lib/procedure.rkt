#lang racket

(#%require "./util.rkt")

(define (make-procedure parameters body env)
  (mlist 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p)
  (make-mutable (cadr p)))

(define (procedure-body p)
  (caddr p))

(define (procedure-environment p)
  (cadddr p))

(provide
  make-procedure
  compound-procedure?
  procedure-parameters
  procedure-body
  procedure-environment)
