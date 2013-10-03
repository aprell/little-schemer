#lang racket

(require rackunit)
(require "01.rkt")

(provide lat?
         member?)

;; Determines whether some list contains only atoms
(define lat?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(atom? (car l)) (lat? (cdr l))]
      [else #f])))

(check-equal? (lat? '()) #t)
(check-equal? (lat? '(a)) #t)
(check-equal? (lat? '(a b c)) #t)
(check-equal? (lat? '(a (b c) d)) #f)

;; Determines whether some atom is a member of a list of atoms
(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [(eq? (car lat) a) #t]
      [else (member? a (cdr lat))])))

(check-equal? (member? 'a '(a b c)) #t)
(check-equal? (member? 'a '(b a c)) #t)
(check-equal? (member? 'a '(b c a)) #t)
(check-equal? (member? 'a '(b c d)) #f)
(check-equal? (member? 'a '()) #f)
