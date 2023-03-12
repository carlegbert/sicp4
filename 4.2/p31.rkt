#lang sicp

(define (tagged-operand? operand tag)
  (and (pair? operand)
       (eq? tag (cadr operand))))

(define (lazy-operand? operand)
  (tagged-operand? operand 'lazy))

(define (lazy-memoized-operand? operand)
  (tagged-operand? operand 'lazy-memo))

(define (memoized-thunk? expr)
  (tagged-list? expr 'memoized-thunk))

(define (thunk? expr)
  (tagged-list? expr 'thunk))

(define (delay-it expr env)
  (cond ((not? (pair? expr))
         ;; if the parameter is not delayed, treat it as
         ;; a memoized thunk that has already been evaluated.
         (list 'evaluated-thunk (actual-value expr env)))
        ((lazy-operand?)
         (list 'thunk expr env))
        ((lazy-memoized-operand? expr)
         (list 'memoized-thunk expr env))
        (else (error "Incompatible argument -- DELAY-IT" expr))))

(define (force-it obj)
  (cond ((memoized-thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        ((thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        (else obj)))
