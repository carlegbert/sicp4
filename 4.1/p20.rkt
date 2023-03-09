#lang racket

(#%require "./lib/random-symbol.rkt")
(#%require "./lib/shared.rkt")
(#%require "./lib/let.rkt")

(define (make-declarations bindings)
  (map (lambda (binding)
         (list (binding-name binding) unassigned))
       bindings))

(define (make-binding-mapping binding intermediary-symbol)
  (list (binding-name binding)
        intermediary-symbol
        (binding-value binding)))

(define (make-declaration binding-mapping)
  (list (car binding-mapping) unassigned))

(define (make-intermediary-binding binding-mapping)
  (cdr binding-mapping))

(define (make-set-exprs binding-mapping)
  (make-set! (car binding-mapping) (cadr binding-mapping)))

(define (letrec->let expr)
  (let* ((get-random (make-random-symbol-generator))
         (binding-mappings
           (map (lambda (binding)
                  (make-binding-mapping binding (get-random)))
                (let-bindings expr)))
         (declarations
           (map make-declaration
                binding-mappings))
         (intermediary-bindings
           (map make-intermediary-binding
                binding-mappings))
         (set-exprs
           (map make-set-exprs
                 binding-mappings))
         (inner-let
           (make-let intermediary-bindings (append set-exprs (let-body expr)))))
    (make-let declarations (list inner-let))))

(define expr-to-be-transformed
  '(letrec
     ((even?
        (lambda (n)
          (if (= n 0)
            #t
            (odd? (- n 1)))))
      (odd?
        (lambda (n)
          (if (= n 0)
            #f
            (even? (- n 1))))))
     (even? 5)
     (odd? 5)))

; (letrec->let expr-to-be-transformed)
;
; formatted result:
; (let ((even? *unassigned*)
;       (odd? *unassigned*))
;   (let ((_a
;           (lambda (n)
;             (if (= n 0)
;               #t
;               (odd? (- n 1)))))
;         (_b
;           (lambda (n)
;             (if (= n 0)
;               #f
;               (even? (- n 1))))))
;     (set! even? _a)
;     (set! odd? _b)
;     (even? 5)
;     (odd? 5)))
