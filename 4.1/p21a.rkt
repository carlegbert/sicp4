#lang racket

((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (ft k)
      (cond ((= k 1) 1)
            ((= k 2) 1)
            (else (+ (ft ft (- k 1))
                     (ft ft (- k 2))))))))
 10)
