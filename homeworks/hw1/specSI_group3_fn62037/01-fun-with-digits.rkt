#lang racket


; Искаме да дефинираме следните имена: one, two, three, ..., nine, plus, minus, times, div,
; така че извиквания от типа на (one (plus (three))) (операция с точно две операнди) да връщат легитимни числови стойности (в този случай - 4)
; Още малко примери:
; (three (times (five))) -> 15
; (nine (div (three))) -> 3
; (eight (minus (four))) -> 4

(define (get-name number)
  (lambda fs (if (null? fs) number ((car fs) number))))

(define (get-op f)
  (lambda (num) (lambda (x) (f x num))))

; numbers
(define one (get-name 1))
(define two (get-name 2))
(define three (get-name 3))
(define four (get-name 4))
(define five (get-name 5))
(define six (get-name 6))
(define seven (get-name 7))
(define eight (get-name 8))
(define nine (get-name 9))

; operations
(define plus (get-op +))
(define times (get-op *))
(define minus (get-op -))
(define div (get-op quotient))

(three (times (five)))
(nine (div (three)))
(eight (minus (four)))
(one (plus (three)))