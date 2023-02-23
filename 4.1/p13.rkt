#lang racket

(#%require "./lib/frame-table.rkt")

(define (make-unbound! var env)
  (cond ((eq? the-empty-environment env)
         (error "Unbound variable -- MAKE-UNBOUND!" var))
        ((((first-frame env) 'remove!) var)
         'ok)
        (else
          (make-unbound! var (enclosing-environment env)))))

;; Although allowing inner frames to unbind
;; values in enclosing environments seems dubious
;; (it makes every definition, ever, mutable!),
;; it's actually pretty limiting to only be able
;; to unbind within a frame. Personally I'll argue
;; that make-unbound! should not exist in the first
;; place, and that anything that's supposed to be
;; mutable should be explicitly defined as such
;; with a let or some other construct (e.g. mcons).
