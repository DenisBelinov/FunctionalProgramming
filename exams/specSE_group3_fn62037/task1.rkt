#lang racket

; graph and assoc implementation from Software Eng excercises
(define (make-alist f keys)
  (map (lambda (key)
         (cons key (f key)))
       keys))
(define (keys alist)
  (map car alist))
(define (values alist)
  (map cdr alist))
; (assoc key alist), (assv key alist), (assq key alist)

(define (del-assoc key alist)
  (filter (lambda (kv)
            (not (equal? (car kv) key)))
          alist))

(define (add-assoc key value alist)
  (cons (cons key value)
        (del-assoc key alist)))

(define empty-graph '())

(define (make-graph vs)
  (make-alist (lambda (_) '())
              vs))

(define vertices keys)

(define (children v g)
  (cdr (assoc v g)))

(define (edge? u v g)
  (member v (children u g)))

(define (map-children v f g)
  (map f (children v g)))

(define (search-child v p g)
  (any? p (children v g)))

(define (add-vertex v g)
  (if (assoc v g)
      g
      (add-assoc v '() g)))

(define (add-if-missing x l)
  (if (member x l)
      l
      (cons x l)))

(define (add-edge u v g)
  (let ((g-with-u-v (add-vertex v (add-vertex u g))))
    (add-assoc u
               (add-if-missing v (children u g-with-u-v))
               g-with-u-v)))

(define (remove-edge u v g)
  (add-assoc u
             (filter (lambda (u)
                       (not (equal? u v)))
                     (children u g))
             g))

(define (parents v g)
  (filter (lambda (x) (edge? x v g)) (vertices g)))


(define (all? p? xs)
  (foldr (lambda (x y) (and x y)) true (map p? xs)))

(define (any? p? xs)
  (not (all? (lambda (x) (not (p? x))) xs)))

(define (are-all-children-in-set v F graph)
  (all? (lambda (x) (member x F)) (children v graph)))

(define (are-no-children-in-set v F graph)
  (not (any? (lambda (x) (member x F)) (children v graph))))

(define (are-all-parents-in-set v F graph)
  (all? (lambda (x) (member x F)) (parents v graph)))

(define (are-no-parents-in-set v F graph)
  (not (any? (lambda (x) (member x F)) (parents v graph))))
  

(define (isInFamily v F graph)
  (or (and (are-all-children-in-set v F graph) (are-no-parents-in-set v F graph))
      (and (are-no-children-in-set v F graph) (are-all-parents-in-set v F graph))))
      
(define (isFamily F graph)
  (all? (lambda (x) x) (map (lambda (v) (isInFamily v F graph)) F)))

(define g (add-edge 'a 'c
                    (add-edge 'a 'b
                              (add-edge 'b 'e
                                        (add-edge 'd 'f    
                                                (make-graph '(a b c d e f)))))))
;(isFamily '(c b d f) g)

(define (uniq l)
  (if (null? l)
      '()
      (cons (car l) (uniq (filter (lambda (x) (not (eq? x (car l)))) l)))))

(define (diff? l1 l2)
  (or (not (null? (filter (lambda (x) (not (member x l2))) l1))))
      (not (null? (filter (lambda (x) (not (member x l1))) l2))))

(define (flat-map f l)
  (apply append (map f l)))

(define (splitFamily F graph)
  (define (add-children f1 f2 graph)
    (and (not (null? f1))
         (not (null? f2))
         (let* ((new-f1 (uniq (flat-map (lambda (v) (children v graph)) f1)))
                (new-f2 (uniq (filter (lambda (v) (not (member v new-f1))) f2))))
           (if (or (diff? new-f1 f1) (diff? new-f2 f2))
               (add-parents (uniq new-f1) (uniq new-f2) graph)
               (list f1 f2)))))

  (define (add-parents f1 f2 graph)
    (and (not (null? f1))
         (not (null? f2))
         (let* ((new-f1 (uniq (flat-map (lambda (v) (parents v graph)) f1)))
                (new-f2 (uniq (filter (lambda (v) (not (member v new-f1))) f2))))
           (if (or (diff? new-f1 f1) (diff? new-f2 f2))
               (add-children new-f1 new-f2 graph)
               (list f1 f2)))))

  (add-children (list (car F)) (cdr F) graph))

(splitFamily '(c b d f) g)