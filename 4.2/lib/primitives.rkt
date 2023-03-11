#lang sicp

(#%require "./util.rkt")

(define true #t)
(define false #f)
(define (true? expr) (eq? true expr))
(define (false? expr) (eq? false expr))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list 'exp exp)
        (list 'length length)
        (list 'list list)
        (list 'list? list?)
        (list 'pair? pair?)
        ;; add more as needed!
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(#%require (only racket provide))

(provide
  primitive-procedure-names
  apply-primitive-procedure
  primitive-procedure?
  primitive-procedure-objects
  true
  true?
  false
  false?)
