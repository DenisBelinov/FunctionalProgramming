(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

; 1 a)
(define (1+ n) (+ n 1))

(define (sum-common-divisors a b)
  (define (id-if-common-divisor x)
    (if (and (= (remainder a x) 0) (= (remainder b x) 0))
        x
        0))
  (accumulate + 0 1 a id-if-common-divisor 1+))

;(sum-common-divisors 14 28)

; 1 b)
(define (greatest-sum a b)

  (define (my-term x)
    (define (sum-common-divisors-wrapped number)
      (if (= x number)
          0
          (sum-common-divisors x number)))
    
    (accumulate max 1 a b sum-common-divisors-wrapped 1+))
  
  (accumulate max 1 a b my-term 1+))

;(greatest-sum 21 34)



; 2
(define (count-metrics ml ll)
  (define (get-results-from-metric m ll) ; replaced with map
    (if (null? ll)
        '()
        (cons (m (car ll)) (get-results-from-metric m (cdr ll)))))
  
  (define (is-metric-correct? m ll)
    (apply = (map m ll)))

  (if (null? ml)
      0
      (if (is-metric-correct? (car ml) ll)
          (+ 1 (count-metrics (cdr ml) ll))
          (count-metrics (cdr ml) ll))))


(define (prod l) (apply * l))
(define (sum l) (apply + l))
;(count-metrics (list sum prod) '((0 1 2) (3 0 5) (1337 0)))
;(count-metrics (list car sum) '((42 -2 2) (42 0) (42)))

; 3
(define (level-flatten dl)
  (define (flatten dl level)
    (if (null? dl)
        dl
        (if (not (list? (car dl)))
            (cons (+ (car dl) level) (flatten (cdr dl) level)) ; we remain on the same level
            (append (flatten (car dl) (+ level 1)) (flatten (cdr dl) level)))))
  (flatten dl 1))

;(level-flatten '(1 (2 3) 4 (5 (6)) (7) ()))