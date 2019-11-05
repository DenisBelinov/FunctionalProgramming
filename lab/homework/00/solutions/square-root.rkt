#lang racket

(provide my-sqrt)

(define (my-sqrt x)
  (define ITERATION_COUNT 10)
  
  (define (func y)
    (- (* y y) x))

  (define (func-der y)
    (* 2.0 y))

  (define (step s i)
    (if (= i 0)
        s
        (step (- s (/ (func s) (func-der s))) (- i 1))))
  
  (step 10 ITERATION_COUNT))