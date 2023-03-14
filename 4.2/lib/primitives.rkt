#lang sicp

(#%require "./util.rkt")

;; I am begging you to tell me how to check reference
;; equality in racket in a way that doesn't feel like
;; a horrible fucking hack
(define cons-symbol
  (lambda () '()))

(define true #t)
(define false #f)
(define (true? expr) (eq? true expr))
(define (false? expr) (eq? false expr))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (lambda-cons a b)
  (cons cons-symbol
        (lambda (f) (f a b))))

(define (lambda-pair? p)
  (and (pair? p)
       (eq? (car p) cons-symbol)))

(define (lambda-car p)
  (if (lambda-pair? p)
    ((cdr p) (lambda (a b) a))
    (error "Expected pair -- CAR")))

(define (lambda-cdr p)
  (if (lambda-pair? p)
    ((cdr p) (lambda (a b) b))
    (error "Expected pair -- CAR")))

(define (lambda-list . items)
  (if (null? items)
    '()
    (lambda-cons (car items)
                 (lambda-list (cdr items)))))

(define (lambda-length items)
  (if (null? items)
    0
    (+ 1 (lambda-length (lambda-cdr items)))))

(define (lambda-list? items)
  (and (lambda-pair? items)
       (or (null? (lambda-cdr items))
           (lambda-list? (lambda-cdr items)))))

(define primitive-procedures
  (list (list 'car lambda-car)
        (list 'cdr lambda-cdr)
        (list 'cons lambda-cons)
        (list 'length lambda-length)
        (list 'list lambda-list)
        (list 'pair? lambda-pair?)
        (list 'list? lambda-list?)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list 'exp exp)
        ;; add more as needed!
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (private-display-pair pair depth)
  (cond ((null? pair)
         (display '()))
        ((eq? depth 5)
         (display '..))
        (else
          (let ((_car (lambda-car pair))
                (_cdr (lambda-cdr pair)))
            (display "(")
            (if (lambda-pair? _car)
              (private-display-pair _car (+ 1 depth))
              (display _car))
            (display " , ")
            (if (lambda-pair? _cdr)
              (private-display-pair _cdr (+ 1 depth))
              (display _cdr))
            (display ")")))))

(define (display-pair pair)
  (private-display-pair pair 0))

(#%require (only racket provide))

(provide
  primitive-procedure-names
  apply-primitive-procedure
  primitive-procedure?
  primitive-procedure-objects
  true
  true?
  false
  false?
  lambda-pair?
  display-pair)
