#lang racket

(require rackunit)
(require "01.rkt"
         "04.rkt")

(provide numbered?
         operator
         1st-sub-exp
         2nd-sub-exp
         value)

;; Determines whether arithemtic expression aexp contains only numbers
;; besides arithmetic operators
(define numbered?
  (lambda (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))])))

(check-equal? (numbered? 3) #t)
(check-equal? (numbered? '(3 + 4)) #t)
(check-equal? (numbered? '(3 + (4 * 5))) #t)
(check-equal? (numbered? '(2 ^ sausage)) #f)

;; Returns the operator of an arithmetic expression  
(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(check-equal? (operator '(3 + 4)) '+)
(check-equal? (operator '((3 + 4) - 2)) '-)

;; Returns the first subexpression of an arithmetic expression  
(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(check-equal? (1st-sub-exp '(3 + 4)) 3)
(check-equal? (1st-sub-exp '((3 + 4) - 2)) '(3 + 4))

;; Returns the second subexpression of an arithmetic expression  
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(check-equal? (2nd-sub-exp '((3 + 4) - 2)) 2)
(check-equal? (2nd-sub-exp '(3 + (4 * (5 ^ 2)))) '(4 * (5 ^ 2)))

;; Returns the value of numbered arithmetic expression nexp (evaluates nexp)
(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (operator nexp) '+)
       (plus (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) '-)
       (minus (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) '*)
       (mult (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) '^)
       (pow (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))])))
      
(check-equal? (value 3) 3)
(check-equal? (value '((3 + 4) - 2)) 5)
(check-equal? (value '(3 + (4 * (5 ^ 2)))) 103)