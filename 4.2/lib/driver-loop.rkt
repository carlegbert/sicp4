#lang sicp

(#%require "./procedure.rkt")
(#%require "./eval.rkt")
(#%require "./environment.rkt")
(#%require "./primitives.rkt")

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval output:")

(define (prompt-for-input str)
  (newline)
  (newline)
  (display str)
  (newline))

(define (announce-output str)
  (newline)
  (display str)
  (newline))

(define (user-print object)
  (cond ((lambda-pair? object)
         (display-pair object))
        ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        (else (display object))))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let* ((input (read))
         (output
           (actual-value input the-global-environment)))
    (announce-output output-prompt)
    (user-print output))
  (driver-loop))

(#%require (only racket provide))

(provide driver-loop)
