#lang racket

(require rnrs/mutable-pairs-6)

(define (enclosing-environment env)
  (cdr env))

(define (first-frame env)
  (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables values))

(define (frame-variables frame)
  (mcar frame))

(define (frame-values frame)
  (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (mcons var (mcar frame)))
  (set-cdr! frame (mcons val (mcdr frame))))

;; returns a pair representing the head of the
;; frame where the variable is bound.
(define (scan-frame var frame)
  (define (iter vars vals)
    (cond ((null? vars)
           #f)
          ((eq? (car vars) var)
           (cons vars vals)
          (else
            (iter (cdr vars) (cdr vals)))))
  (iter (frame-vars frame) (frame-vals frame)))

;; returns a pair representing the head of the
;; frame where the variable is bound.
(define (scan-env var env)
  (if (eq? the-empty-environment env)
    #f
    (let ((binding (scan-frame var (first-frame env))))
      (if (not binding)
        (scan-env var (enclosing-environment env))
        binding))))

(define (update-binding! binding new-val)
  (set-car! (cdr binding) new-val))

(define (lookup-variable-value var env)
  (let ((binding (scan-env var env)))
    (if (not binding)
        (error "Unbound variable" var)
        (cadr binding))))

(define (set-variable-value! var val env)
  (let ((binding (scan-env var env)))
    (if (not binding)
      (error "Unbound variable -- SET" var)
      (begin
        (update-binding! binding val)
        'ok))))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (binding (scan-frame var frame)))
    (if (not binding)
      (add-binding-to-frame! var val frame)
      (update-binding! binding val))))

(provide
  lookup-variable-value
  set-variable-value!
  define-variable!)
