#lang racket

(require rackunit)
(require "01.rkt")

(provide plus
         minus
         mult
         div
         pow
         addtup
         tup+
         greater-than?
         less-than?
         equals?
         equan?
         length
         pick
         rempick
         no-nums
         all-nums
         occur
         one?)

;; n + m
(define plus
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (add1 (plus n (sub1 m)))])))

(check-equal? (plus 0  2)  2)
(check-equal? (plus 4  0)  4)
(check-equal? (plus 6  3)  9)
(check-equal? (plus 8 12) 20)

;; n - m
(define minus
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (sub1 (minus n (sub1 m)))])))

(check-equal? (minus  2  2) 0)
(check-equal? (minus  4  0) 4)
(check-equal? (minus  6  3) 3)
(check-equal? (minus 12 10) 2)

;; n * m
(define mult
  (lambda (n m)
    (cond
      [(zero? m) 0]
      [else (plus n (mult n (sub1 m)))])))

(check-equal? (minus  2  2) 0)
(check-equal? (minus  4  0) 4)
(check-equal? (minus  6  3) 3)
(check-equal? (minus 12 10) 2)

;; Sums all the numbers in a tuple (list of nonnegative numbers)
(define addtup
  (lambda (tup)
    (cond
      [(null? tup) 0]
      [else (plus (car tup) (addtup (cdr tup)))])))

(check-equal? (addtup '(0 0 1 0))  1)
(check-equal? (addtup '(3 5 2 8)) 18)
(check-equal? (addtup '(15 6 7 12 3)) 43)
(check-equal? (addtup '(5 5 5 5)) (mult 5 4))

;; Builds a new tuple t with element t(i) = tup1(i) + tup2(i)
;; If tup1 and tup2 are not the same length, t will contain the remaining
;; elements of the longer tuple
(define tup+
  (lambda (tup1 tup2)
    (cond
      [(null? tup1) tup2]
      [(null? tup2) tup1]
      [else (cons (plus (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))])))

(check-equal? (tup+ '(3 6 9 11 4) '(8 5 2 0 7)) '(11 11 11 11 11))
(check-equal? (tup+ '(2 3) '(4 6)) '(6 9))
(check-equal? (tup+ '(3 7) '(4 6 8 1)) '(7 13 8 1))
(check-equal? (tup+ '(3 7 8 1) '(4 6)) '(7 13 8 1))

;; Returns true if n > m, false otherwise 
(define greater-than?
  (lambda (n m)
    (cond
      [(zero? n) #f]
      [(zero? m) #t]
      [else (greater-than? (sub1 n) (sub1 m))])))

(check-equal? (greater-than? 12 133) #f)
(check-equal? (greater-than? 120 11) #t)
(check-equal? (greater-than? 3 3) #f)

;; Returns true if n < m, false otherwise 
(define less-than?
  (lambda (n m)
    (cond
      [(zero? m) #f]
      [(zero? n) #t]
      [else (less-than? (sub1 n) (sub1 m))])))

(check-equal? (less-than? 4 6) #t)
(check-equal? (less-than? 8 3) #f)
(check-equal? (less-than? 6 6) #f)

;; Returns true if n = m, false otherwise
(define equals?
  (lambda (n m)
    (cond
      [(or (> n m) (< n m)) #f]
      [else #t])))

(check-equal? (equals? 2 3) #f)
(check-equal? (equals? 7 4) #f)
(check-equal? (equals? 8 8) #t)

;; n^m
(define pow
  (lambda (n m)
    (cond
      [(zero? m) 1]
      [else (mult n (pow n (sub1 m)))])))

(check-equal? (pow 1 1) 1)
(check-equal? (pow 2 3) 8)
(check-equal? (pow 5 3) 125)

;; n / m
(define div
  (lambda (n m)
    (cond
      [(< n m) 0]
      [else (add1 (div (minus n m) m))])))

(check-equal? (div 15 4) 3)
(check-equal? (div 10 2) 5)
(check-equal? (div 3 4) 0)

;; Determines the number of atoms in lat (length of lat)
(define length
  (lambda (lat)
    (cond
      [(null? lat) 0]
      [else (add1 (length (cdr lat)))])))

(check-equal? (length '(a b c d)) 4)

;; Picks the nth element from lat
(define pick
  (lambda (n lat)
    (cond
      [(= n 1) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))

(check-equal? (pick 1 '(a b c)) 'a)
(check-equal? (pick 3 '(a b c)) 'c)

;; Removes the nth element from lat
(define rempick
  (lambda (n lat)
    (cond
      [(= n 1) (cdr lat)]
      [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])))

(check-equal? (rempick 1 '(a b c d)) '(b c d))
(check-equal? (rempick 3 '(a b c d)) '(a b d))

;; Removes all numbers from lat, keeps all nonnumeric atoms
(define no-nums
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(number? (car lat)) (no-nums (cdr lat))]
      [else (cons (car lat) (no-nums (cdr lat)))])))

(check-equal? (no-nums '(a 1 b 2 c d)) '(a b c d))
(check-equal? (no-nums '(a 1 2 b c d)) '(a b c d))
(check-equal? (no-nums '(a b c d)) '(a b c d))
(check-equal? (no-nums '(1 2 3 4)) '())

;; Removes all nonnumeric atoms from lat, keeps all numbers
(define all-nums
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(not (number? (car lat))) (all-nums (cdr lat))]
      [else (cons (car lat) (all-nums (cdr lat)))])))

(check-equal? (all-nums '(a 1 b 2 c d)) '(1 2))
(check-equal? (all-nums '(a 1 2 b c d)) '(1 2))
(check-equal? (all-nums '(a b c d)) '())
(check-equal? (all-nums '(1 2 3 4)) '(1 2 3 4))

;; Returns true if a1 = a2, false otherwise
;; a1 and a2 can be any atom, numeric or nonnumeric
(define equan?
  (lambda (a1 a2)
    (cond
      [(and (number? a1) (number? a2)) (= a1 a2)]
      [(or  (number? a1) (number? a2)) #f]
      [else (eq? a1 a2)])))

(check-equal? (equan? 'a 'a) #t)
(check-equal? (equan? 'a 'b) #f)
(check-equal? (equan? 'a  2) #f)
(check-equal? (equan?  2  2) #t)
(check-equal? (equan?  2  5) #f)

;; Returns the number of occurrences of a in lat
(define occur
  (lambda (a lat)
    (cond
      [(null? lat) 0]
      [(eq? (car lat) a) (add1 (occur a (cdr lat)))]
      [else (occur a (cdr lat))])))

(check-equal? (occur 'a '(a b c d)) 1)
(check-equal? (occur 'a '(a b a d)) 2)
(check-equal? (occur 'a '(b c d e)) 0)
(check-equal? (occur 'a '(a a a a)) (length '(a a a a)))

;; Returns true if n = 1, false otherwise
(define one?
  (lambda (n)
    (= n 1)))

(check-equal? (one? 1) #t)
(check-equal? (one? 2) #f)