#lang racket
(provide make-date day month year next-day date->string date<)

;utility functions
(define (all? p? l)
  (if (null? l)
      #t
      (and (p? (car l)) (all? p? (cdr l)))))

; core date functions
(define SHORT_MONTHS '(4 6 9 11))
(define LONG_MONTHS '(1 3 5 7 8 10 12))

(define (make-date d m y)
  (list d m y))

(define (day date)
  (car date))

(define (month date)
  (cadr date))

(define (year date)
  (caddr date))

(define (is-leap? y)
  (or (= (remainder y 400) 0)
      (and (= (remainder y 4) 0) (not (= (remainder y 100) 0)))))


(define (verify-month month)
  (and (>= month 1) (<= month 12)))

(define (verify-day day month is-leap)
  (if (< day 0)
      #f
      (case month
        [(4 6 9 11) (<= day 30)]
        [(1 3 5 7 8 10 12) (<= day 31)]
        [else (if is-leap
                  (<= day 29)
                  (<= day 28))])))

(define (verify date)
  (let ((is-leap (is-leap? (year date))))
    (and (verify-month (month date))
         (verify-day (day date) (month date) is-leap))))

(define (date? date)
  (and (list? date)
       (= (length date) 3)
       (all? number? date)
       (verify date)))

; work with dates
(define (date->string date)
  (define d (number->string (day date)))
  (define m (number->string (month date)))
  (define y (number->string (year date)))
  (string-append d "." m "." y))

(define (next-day date)
  (define d (day date))
  (define m (month date))
  (define y (year date))
  (if (date? (make-date (+ 1 d) m y))
      (make-date (+ 1 d ) m y)
      (if (date? (make-date 1 (+ 1 m) y))
          (make-date 1 (+ 1 m) y)
          (make-date 1 1 (+ 1 y)))))

(define (date< date1 date2)
  (define year-dif (- (year date1) (year date2)))
  (define month-dif (- (month date1) (month date2)))
  (define day-dif (- (day date1) (day date2)))
  (if (not (= year-dif 0))
      (negative? year-dif)
      (if (not (= month-dif 0))
          (negative? month-dif)
          (negative? day-dif))))

