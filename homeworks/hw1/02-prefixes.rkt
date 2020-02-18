#lang racket


; Искаме да намерим всички префикси на даден списък
; Например за '(1 2 3), това са '(), '(1), '(1 2) и '(1 2 3)

(define (prefixes xs)
  (if (null? xs)
      (list xs)
      (cons '()
            (map (lambda (x) (cons (car xs) x))
                 (prefixes (cdr xs))))))


(prefixes '(2 1 '(1 2 3)))