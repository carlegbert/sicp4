#lang racket

(define (make-random-symbol-generator)
    (let ((random-symbols
            ;; well, no, they're not random whatsoever, but for
            ;; our purposes it should be easy to be pretend.
            '(_head _a _b _c _d _e _f _g _h _i _j _k _l _m
                    _n _o _p _q _r _s _t _u _v _w _x _y _z)))
      (lambda ()
        (set! random-symbols (cdr random-symbols))
        (car random-symbols))))

(provide make-random-symbol-generator)
