#lang racket

(require rackunit)
(require "01.rkt"
         "04.rkt"
         "07.rkt")

(provide rember-f
         insertL-f
         insertR-f
         multirember-f
         multirember&co
         multiinsertLR
         multiinsertLR&co)

;; Variation of rember that takes a comparison function as an argument
(define rember-ff
  (lambda (test? a lat)
    (cond
      [(null? lat) '()]
      [(test? (car lat) a) (cdr lat)]
      [else (cons (car lat) (rember-ff test? a (cdr lat)))])))

(check-equal? (rember-ff = '1 '()) '())
(check-equal? (rember-ff = '1 '(1 2 3)) '(2 3))
(check-equal? (rember-ff eq? 'a '(b a c)) '(b c))
(check-equal? (rember-ff eq? 'a '(b a c a)) '(b c a))

;; Returns rember that uses comparison function test?
(define rember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) '()]
        [(test? (car lat) a) (cdr lat)]
        [else (cons (car lat) ((rember-f test?) a (cdr lat)))]))))

(define rember-eq? (rember-f eq?))

(check-equal? (rember-eq? 'a '()) '())
(check-equal? (rember-eq? 'a '(b c)) '(b c))
(check-equal? (rember-eq? 'a '(b a c)) '(b c))
(check-equal? (rember-eq? 'a '(b a c a)) '(b c a))

;; Returns insertL that uses comparison function test?
(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        [(null? lat) '()]
        [(test? (car lat) old) (cons new lat)]
        [else (cons (car lat) ((insertL-f test?) new old (cdr lat)))]))))

(define insertL-eq? (insertL-f eq?))

(check-equal? (insertL-eq? 'b 'a '(a c)) '(b a c))
(check-equal? (insertL-eq? 'b 'a '(a c a)) '(b a c a))
(check-equal? (insertL-eq? 'c 'b '(a b c)) '(a c b c))
(check-equal? (insertL-eq? 'c 'b '(c d e f)) '(c d e f))

;; Returns insertR that uses comparison function test?
(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        [(null? lat) '()]
        [(test? (car lat) old) (cons old (cons new (cdr lat)))]
        [else (cons (car lat) ((insertR-f test?) new old (cdr lat)))]))))

(define insertR-eq? (insertR-f eq?))

(check-equal? (insertR-eq? 'b 'a '(a c)) '(a b c))
(check-equal? (insertR-eq? 'b 'a '(a c a)) '(a b c a))
(check-equal? (insertR-eq? 'c 'b '(a b c)) '(a b c c))
(check-equal? (insertR-eq? 'c 'b '(c d e f)) '(c d e f))

;; Returns multirember that uses comparison function test?
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) '()]
        [(test? (car lat) a) ((multirember-f test?) a (cdr lat))]
        [else (cons (car lat) ((multirember-f test?) a (cdr lat)))]))))

(define multirember-eq? (multirember-f eq?))

(check-equal? (multirember-eq? 'a '()) '())
(check-equal? (multirember-eq? 'a '(b c)) '(b c))
(check-equal? (multirember-eq? 'a '(b a c)) '(b c))
(check-equal? (multirember-eq? 'a '(b a c a)) '(b c))

;; Variation of multirember that separates the atoms in lat
;; All occurences of a are collected in lat2,
;; all other atoms are collected in lat1
(define multirember&co 
  (lambda (a lat col)
    (cond
      [(null? lat) 
       (col '() '())]
      [(eq? (car lat) a)
       (multirember&co a (cdr lat) 
         (lambda (lat1 lat2) 
           (col lat1 (cons (car lat) lat2))))]
      [else
       (multirember&co a (cdr lat)
         (lambda (lat1 lat2)
           (col (cons (car lat) lat1) lat2)))])))

(define count
  (lambda (lat1 lat2)
    (length lat2)))

(check-equal? (multirember&co 'a '() count) 0)
(check-equal? (multirember&co 'a '(b c) count) 0)
(check-equal? (multirember&co 'a '(b a c) count) 1)
(check-equal? (multirember&co 'a '(b a c a) count) 2)
(check-equal? (multirember&co 'a '() build) '(() ()))
(check-equal? (multirember&co 'a '(b c) build) '((b c) ()))
(check-equal? (multirember&co 'a '(b a c) build) '((b c) (a)))
(check-equal? (multirember&co 'a '(b a c a) build) '((b c) (a a)))

;; Inserts new to the left of oldL and to the right of oldR
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond 
      [(null? lat) '()]
      [(eq? (car lat) oldL)
       (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat))))]
      [(eq? (car lat) oldR)
       (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat))))]
      [else 
       (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))])))
      
(check-equal? (multiinsertLR 'a 'b 'c '(a d)) '(a d))
(check-equal? (multiinsertLR 'a 'b 'c '(b c d)) '(a b c a d))
(check-equal? (multiinsertLR 'a 'b 'c '(b b d c e)) '(a b a b d c a e))
(check-equal? (multiinsertLR 'a 'b 'b '(b b c)) '(a b a b c))

;; Variation of multiinsertLR that collects the number of insertions
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      [(null? lat)
       (col '() 0 0)]
      [(eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
         (lambda (newlat nl nr)
           (col (cons new (cons oldL newlat)) (add1 nl) nr)))]
      [(eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
         (lambda (newlat nl nr)
           (col (cons oldR (cons new newlat)) nl (add1 nr))))]
      [else
       (multiinsertLR&co new oldL oldR (cdr lat)
         (lambda (newlat nl nr)
           (col (cons (car lat) newlat) nl nr)))])))

(define mybuild
  (lambda (lat nl nr)
    (build nl nr)))

(check-equal? (multiinsertLR&co 'a 'b 'c '(a d) mybuild) '(0 0))
(check-equal? (multiinsertLR&co 'a 'b 'c '(b c d) mybuild) '(1 1))
(check-equal? (multiinsertLR&co 'a 'b 'c '(b b d c e) mybuild) '(2 1))
(check-equal? (multiinsertLR&co 'a 'b 'b '(b b c) mybuild) '(2 0))
