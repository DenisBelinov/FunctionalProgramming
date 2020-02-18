#lang racket
(require "./03-dates.rkt")

;(1461 × (Y + 4800 + (M - 14)/12))/4 +(367 × (M - 2 - 12 × ((M - 14)/12)))/12 - (3 × ((Y + 4900 + (M - 14)/12)/100))/4 + D - 32075
(define (get-julian-day-number date)
  (define d (day date))
  (define m (month date))
  (define y (year date))

  (+ (quotient (* 1461
                  (+ y 4800 (quotient (- m 14) 12)))
               4)
     (quotient (* 367
                  (- m
                     2
                     (* 12
                        (quotient (- m 14)
                                  12))))
               12)
     (- (quotient (* 3
                  (quotient (+ y
                               4900
                               (quotient (- m 14) 12))
                            100))
               4))
     d
     -32075))

(define (weekday date)
  (define jn (get-julian-day-number date))
  (define week-day (+ (remainder jn 7) 1))

  (case week-day
    ((1) 'Monday)
    ((2) 'Tuesday)
    ((3) 'Wednesday)
    ((4) 'Thursday)
    ((5) 'Friday)
    ((6) 'Saturday)
    ((7) 'Sunday)))
      
;(weekday (make-date 8 12 2019))

(define (next-weekday wd date)
  (if (equal? (weekday date) wd)
      date
      (next-weekday wd (next-day date))))