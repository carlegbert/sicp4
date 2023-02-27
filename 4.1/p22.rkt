#lang racket

;; let being syntactic sugar should mean
;; that relying on analyze-lambda is perfectly
;; sufficient, unless I'm missing something.
(define (analyze-let expr)
  (analyze (let->combination expr)))
