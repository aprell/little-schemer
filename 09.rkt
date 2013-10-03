#lang racket

(require rackunit)
(require "01.rkt"
         "04.rkt"
         "07.rkt")

(provide shift
         align
         length*)

;; Takes a pair whose first component is a pair and builds a new pair
;; by shifting the second part of the first component into the second component
;; A pair is a list with two S-expressions 
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(check-equal? (shift '((a b) c)) '(a (b c)))
(check-equal? (shift '((a b) (c d))) '(a (b (c d))))
(check-equal? (shift '((a (b)) c)) '(a ((b) c)))

;; ???
(define align
  (lambda (pora)
    (cond
      [(atom? pora) pora]
      [(a-pair? (first pora)) (align (shift pora))]
      [else (build (first pora) (align (second pora)))])))

;; Counts the number of atoms in a pair
(define length*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else (plus (length* (first pora)) 
                  (length* (second pora)))])))

(check-equal? (length* '(a b)) 2)
(check-equal? (length* '((1 2) a)) 3)
(check-equal? (length* '((a (b 1)) (c (2 3)))) 6)