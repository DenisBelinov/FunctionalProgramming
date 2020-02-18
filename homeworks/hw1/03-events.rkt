#lang racket
(require "./03-dates.rkt")

; assoc array utils, copied from the lectures
(define (del-assoc key al)
  (filter (lambda (kv) (not (equal? (car kv) key))) al))

(define (add-assoc key value al)
  (cons (cons key value) (del-assoc key al)))

; task
(define (events-for-day date events)
  (if (null? events)
      '()
      (let* ((event (car events))
             (event-date (car event))
             (event-date-string (date->string event-date))
             (date-string (date->string date))
             (rec (events-for-day date (cdr events))))
        
        (if (equal? event-date-string date-string)
            (cons event rec)
            rec))))

; sort from https://stackoverflow.com/questions/50182247/sorting-a-list-by-the-car-of-each-element-in-racket
(define (sort-calendar calendar)
  (sort calendar date< #:key car))

(define (add-event-to-calendar event calendar)
  (define event-date (car event))
  (define event-value (cdr event))
  (define existing-kv (assoc event-date calendar))
  
  (if existing-kv
      (add-assoc event-date
                 (cons event-value (cdr existing-kv))
                 calendar)
      (add-assoc event-date (list event-value) calendar)))

(define (calendar events)
  (if (null? events)
      '()
      (let* ((curr-event (car events))
             (rec (calendar (cdr events))))
        (sort-calendar (add-event-to-calendar curr-event rec)))))

(calendar (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                (cons (make-date 25 12 2019) "Коледа")
                (cons (make-date 27 11 2019) "Спират водата в Младост")
                (cons (make-date 23 3 2018) "Концерт на Лепа Брена")))
            