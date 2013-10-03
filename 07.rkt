#lang racket

(require rackunit)
(require "01.rkt"
         "02.rkt"
         "03.rkt")

(provide set?
         makeset
         subset?
         eqset?
         intersect?
         intersect
         union
         difference
         a-pair?
         first
         second
         build
         fun?
         revpair
         revrel
         one-to-one?)

;; Determines whether lat is a set (no atom occurs more than once)
(define set?
  (lambda (lat)
    (cond
      [(null? lat) #t]
      [(member? (car lat) (cdr lat)) #f]
      [else (set? (cdr lat))])))

(check-equal? (set? '(a b c)) #t)
(check-equal? (set? '(1 2 3)) #t)
(check-equal? (set? '(a b 1 a c 2)) #f)
(check-equal? (set? '()) #t)

;; Makes a set out of lat by removing all duplicates
(define makeset
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(member? (car lat) (cdr lat)) (makeset (cdr lat))]
      [else (cons (car lat) (makeset (cdr lat)))])))
      
(check-equal? (makeset '(a b a c b)) '(a c b))
(check-equal? (makeset '(a 1 2 b 3)) '(a 1 2 b 3))
(check-equal? (set? (makeset '(a 1 a b c))) #t)

;; Determines whether set1 is a subset of set2
(define subset?
  (lambda (set1 set2)
    (cond
      [(null? set1) #t]
      [(member? (car set1) set2) (subset? (cdr set1) set2)]
      [else #f])))

(check-equal? (subset? '(a b) '(a c d b 1)) #t)
(check-equal? (subset? '(a b c 2 3 d) '(a c d b 1)) #f)
(check-equal? (subset? '() '(a b c)) #t)

;; Determines whether two sets are equal
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(check-equal? (eqset? '(a b c) '(b c a)) #t)
(check-equal? (eqset? '(1 2 3) '(1 2 3 4)) #f)
(check-equal? (eqset? '() '()) #t)

;; Determines whether set1 intersects set2
(define intersect?
  (lambda (set1 set2)
    (cond
      [(null? set1) #f]
      [(member? (car set1) set2) #t]
      [else (intersect? (cdr set1) set2)])))

(check-equal? (intersect? '(a b c) '(b 1 2 d)) #t)
(check-equal? (intersect? '(a b c) '(d e f)) #f)
(check-equal? (intersect? '() '()) #f)

;; Returns the intersection of set1 and set2
(define intersect
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [(member? (car set1) set2) 
       (cons (car set1) (intersect (cdr set1) set2))]
      [else (intersect (cdr set1) set2)])))

(check-equal? (intersect '(a b c 1 3) '(b 1 2 d)) '(b 1))
(check-equal? (intersect '(a 1 2 3) '(3 2 1 a)) '(a 1 2 3))
(check-equal? (intersect '(a b c) '(d e f)) '())

;; Returns the union of set1 and set2
(define union
  (lambda (set1 set2)
    (cond
      [(null? set1) set2]
      [(member? (car set1) set2) (union (cdr set1) set2)]
      [else (cons (car set1) (union (cdr set1) set2))])))

(check-equal? (union '(a b c 1 3) '(b 1 2 d)) '(a c 3 b 1 2 d))
(check-equal? (union '(a 1 2 3) '(3 2 1 a)) '(3 2 1 a))
(check-equal? (union '(a b c) '(d e f)) '(a b c d e f))

;; Returns all the atoms in set1 that are not in set2
(define difference
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [(member? (car set1) set2) (difference (cdr set1) set2)]
      [else (cons (car set1) (difference (cdr set1) set2))])))

(check-equal? (difference '(a b c 1 3) '(b 1 2 d)) '(a c 3))
(check-equal? (difference '(a 1 2 3) '(3 2 1 a)) '())
(check-equal? (difference '(a b c) '(d e f)) '(a b c))

;; Determines whether x is a pair (a list with only two S-expressions)
(define a-pair?
  (lambda (x)
    (cond
      [(atom? x) #f]
      [(null? x) #f]
      [(null? (cdr x)) #f]
      [else (null? (cdr (cdr x)))])))

(check-equal? (a-pair? 'a) #f)
(check-equal? (a-pair? '(a)) #f)
(check-equal? (a-pair? '(a b)) #t)
(check-equal? (a-pair? '((a b) (c))) #t)
(check-equal? (a-pair? '(a (b c) d)) #f)

;; Returns the first S-expression of a pair
(define first
  (lambda (p)
    (car p)))

;; Returns the second S-expression of a pair
(define second
  (lambda (p)
    (car (cdr p))))

;; Builds a pair from two S-expressions
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(check-equal? (build 'a 'b) '(a b))
(check-equal? (build '(1 2) 3) '((1 2) 3))
(check-equal? (build '((a)) '((b (c)))) '(((a)) ((b (c)))))

;; Determines whether relation rel is a function
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(check-equal? (fun? '((0 0) (1 1) (2 4) (3 9))) #t)
(check-equal? (fun? '((0 0) (1 1) (1 4))) #f)
(check-equal? (fun? '((a b) (b c) (c d))) #t)

;; Reverses the two S-expressions in a pair
(define revpair
  (lambda (p)
    (build (second p) (first p))))

(check-equal? (revpair '(a b)) '(b a))
(check-equal? (revpair '((a (b c)) (d 1))) '((d 1) (a (b c))))
(check-equal? (revpair '(() ())) '(() ()))

;; Reverses each pair in a set of pairs
(define revrel
  (lambda (rel)
    (cond
      [(null? rel) '()]
      [else (cons (revpair (car rel)) (revrel (cdr rel)))])))

(check-equal? (revrel '((a (b)) (c d) (1 2))) '(((b) a) (d c) (2 1)))
(check-equal? (revrel '((b a) (d c))) '((a b) (c d)))
(check-equal? (revrel '((() ()) (() ()))) '((() ()) (() ())))

;; Determines whether rel represents a one-to-one mapping
(define one-to-one?
  (lambda (rel)
    (and (fun? rel) (fun? (revrel rel)))))

(check-equal? (one-to-one? '((0 0) (1 1) (2 4) (3 9))) #t)
(check-equal? (one-to-one? '((0 0) (1 1) (1 4))) #f)
(check-equal? (one-to-one? '((a b) (b c) (c b))) #f)