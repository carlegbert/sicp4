#lang racket

(require compatibility/mlist)
(#%require "./util.rkt")

(define true #t)
(define false #f)
(define (true? expr) (eq? true expr))
(define (false? expr) (eq? false expr))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (mcar (mcdr proc)))

(define primitive-procedures
  (mlist (mlist 'car car)
        (mlist 'cdr cdr)
        (mlist 'cons cons)
        (mlist 'null? null?)
        (mlist '+ +)
        (mlist '- -)
        (mlist '* *)
        (mlist 'exp exp)
        (mlist 'length length)
        (mlist 'list list)
        (mlist 'list? list?)
        (mlist 'pair? pair?)
        (mlist 'exit exit)))

(define (primitive-procedure-names)
  (mmap mcar primitive-procedures))

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (mlist 'primitive (mcar (mcdr proc))))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(provide
  primitive-procedure-names
  apply-primitive-procedure
  primitive-procedure?
  primitive-procedure-objects
  true
  true?
  false
  false?)
