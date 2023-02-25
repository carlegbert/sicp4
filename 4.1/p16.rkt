#lang racket

(#%require "./lib/shared.rkt")

(define (enclosing-environment env)
  (cdr env))

(define the-empty-environment '())

(define (first-frame env)
  (car env))

(define (frame-variables frame)
  (mcar frame))

(define (frame-values frame)
  (mcdr frame))

;; a.

(define unassigned '*unassigned*)

(define (unassigned? val)
  (eq? unassigned val))

(define (error-if-unassigned var val)
  (if (unassigned? val)
    (error "Unbound variable" var)
    val))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (error-if-unassigned var (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

;; b.

(define (make-set! var val)
  (list 'set! var val))

(define (function-definition-name definition)
  (caadr definition))

(define (function-definition-parameters definition)
  (cdadr definition))

(define (function-definition-body definition)
  (cddr definition))

(define (define-to-lambda-binding definition)
  (list (function-definition-name definition)
        (make-lambda
          (function-definition-parameters definition)
          (function-definition-body definition))))

(define (make-binding-from-define definition)
  (if (list? (cadr definition))
    (define-to-lambda-binding definition)
    (cdr definition)))

(define (make-assignments-from-bindings bindings)
  (map (lambda (binding) (make-set! (car binding) (cadr binding)))
       bindings))

(define (make-unassigned-bindings bindings)
  (map (lambda (binding) (list (car binding) unassigned))
       bindings))

(define (make-expr-with-replaced-defines
          bindings
          body-exprs)
  (make-let (make-unassigned-bindings bindings)
            (append (make-assignments-from-bindings bindings)
                    body-exprs)))

(define (scan-out-defines procedure-body)
  (define (iter unscanned-exprs body-exprs bindings)
    (cond ((null? unscanned-exprs)
           (make-expr-with-replaced-defines
             bindings
             body-exprs))
          ((definition? (car unscanned-exprs))
           (iter (cdr unscanned-exprs)
                  body-exprs
                 (append bindings
                         (list (make-binding-from-define (car unscanned-exprs))))))
          (else (iter (cdr unscanned-exprs)
                      (append (list (car unscanned-exprs))
                              body-exprs)
                      bindings))))
  (iter procedure-body '() '()))

; (define expr-with-defines
;   '((define one 1)
;     (define (add-one x) (+ x one))
;     (add-one one)))
;
; (scan-out-defines expr-with-defines)
;
; result:
; '(let ((one *unassigned*) (add-one *unassigned*)) (set! one 1) (set! add-one (lambda (x) (+ x one))) (add-one one))
;
; formatted:
; (let ((one *unassigned*) (add-one *unassigned*))
;   (set! one 1)
;   (set! add-one (lambda (x) (+ x one)))
;   (add-one one))

; c.

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
