#lang racket

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (unless-condition expr)
  (cadr expr))

(define (unless-usual-value expr)
  (caddr expr))

(define (unless-exceptional-value expr)
  (cadddr expr))

(define (unless->if expr)
  (make-if (unless-condition expr)
           (unless-exceptional-value expr)
           (unless-usual-value expr)))

; (define simple-expr
;   '(unless #t 'exceptional! 'normal.))
;
; (define lambda-expr
;   '(unless (lambda () #t)
;      (lambda () 'exceptional!)
;      (lambda () 'normal.)))
:
; (unless->if simple-expr)
; (unless->if lambda-expr)
