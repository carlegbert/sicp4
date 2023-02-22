#lang racket

(define (application? exp)
  (tagged-list? exp 'call))

(define (application-args exp)
  (caddr exp))

(define (operands exp)
  (cddr exp))

(define (operator exp)
  (cadr exp))
