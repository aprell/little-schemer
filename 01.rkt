#lang racket

(require rackunit)

(provide atom?
         list?
         s-exp?)

;; Determines whether some value is an atom
;; (pair? determines whether some value is a constructed list)
(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(check-equal? (atom? 'a) #t)
(check-equal? (atom? 1) #t)
(check-equal? (atom? '()) #f)
(check-equal? (atom? '(1)) #f)

;; Determines whether some value is a list
(define list?
  (lambda (l)
    (or (pair? l) (null? l))))

(check-equal? (list? '()) #t)
(check-equal? (list? '(1 2 3)) #t)
(check-equal? (list? '(1 2 (3 4))) #t)
(check-equal? (list? 'a) #f)

;; Determines whether some value is an S-expression
(define s-exp?
  (lambda (s)
    (or (atom? s) (list? s))))

(check-equal? (s-exp? '()) #t)
(check-equal? (s-exp? '(1 2 3)) #t)
(check-equal? (s-exp? '(1 2 (3 4))) #t)
(check-equal? (s-exp? 'a) #t)
