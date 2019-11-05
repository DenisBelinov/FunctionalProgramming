#lang racket

(provide from-k-ary
         to-k-ary)

(define (from-k-ary n k)
  (define (helper number power)
    (if (= number 0)
        0
        (+ (* (remainder number 10) (expt k power))
           (helper (quotient number 10) (+ power 1)))))
  (helper n 0))

(define (to-k-ary n k)
  (define (helper number p)
    (if (= number 0)
        0
        (+ (* (remainder number k) (expt 10 p))
           (helper (quotient number k) (+ p 1)))))
  (helper n 0))
