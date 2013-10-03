#lang racket

(require rackunit)
(require "01.rkt"
         "04.rkt")

(provide rember*
         insertR*
         insertL*
         occur*
         subst*
         member*
         leftmost
         eqlist?)

;; Removes all occurrences of a from list of S-expressions l
(define rember*
  (lambda (a l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? (car l) a) (rember* a (cdr l))]
         [else (cons (car l) (rember* a (cdr l)))])]
      [else (cons (rember* a (car l)) (rember* a (cdr l)))])))

(check-equal? (rember* 'a '((b a) a ((c) a))) '((b) ((c))))
(check-equal? (rember* 'a '((b (c (d))) e)) '((b (c (d))) e))
(check-equal? (rember* 'b '(b a d)) '(a d))

;; Inserts new after every occurrence of old in list of S-expressions l
(define insertR*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? (car l) old) (cons old (cons new (insertR* new old (cdr l))))]
         [else (cons (car l) (insertR* new old (cdr l)))])]
      [else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))])))

(check-equal? (insertR* 'a 'b '(c (b) (d (b e)))) '(c (b a) (d (b a e))))
(check-equal? (insertR* 'a 'b '(b (b b) c)) '(b a (b a b a) c))

;; Inserts new before every occurrence of old in list of S-expressions l
(define insertL*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? (car l) old) (cons new (cons old (insertL* new old (cdr l))))]
         [else (cons (car l) (insertL* new old (cdr l)))])]
      [else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))])))

(check-equal? (insertL* 'a 'b '(c (b) (d (b e)))) '(c (a b) (d (a b e))))
(check-equal? (insertL* 'a 'b '(b (b b) c)) '(a b (a b a b) c))

;; Counts the number of occurences of a in list of S-expressions l
(define occur*
  (lambda (a l)
    (cond
      [(null? l) 0]
      [(atom? (car l))
       (cond
         [(eq? (car l) a) (add1 (occur* a (cdr l)))]
         [else (occur* a (cdr l))])]
      [else (plus (occur* a (car l)) (occur* a (cdr l)))])))

(check-equal? (occur* 'a '(b (c a d) ((a)))) 2)
(check-equal? (occur* 'b '((a (c)) d (((b)) d))) 1)

;; Replaces all occurrences of old with new in list of S-expressions l
(define subst*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? (car l) old) (cons new (subst* new old (cdr l)))]
         [else (cons (car l) (subst* new old (cdr l)))])]
      [else (cons (subst* new old (car l)) (subst* new old (cdr l)))])))

(check-equal? (subst* 'a 'b '(a (c d (b) e) ((b)))) '(a (c d (a) e) ((a))))
(check-equal? (subst* 'b 'a '(d (((c (a)))))) '(d (((c (b))))))

;; Checks whether a appears in list of S-expressions l
(define member*
  (lambda (a l)
    (cond
      [(null? l) #f]
      [(atom? (car l))
       (cond
         [(eq? (car l) a) #t]
         [else (member* a (cdr l))])]
      [else (or (member* a (car l)) (member* a (cdr l)))])))

(check-equal? (member* 'a '(b (c d) ((a b)))) #t)
(check-equal? (member* 'a '(b a (c) ((a)))) #t)
(check-equal? (member* 'b '(a c d (e))) #f)

;; Returns the leftmost atom in non-empty list of S-expressions l
;; We assume that l does not contain empty lists
(define leftmost
  (lambda (l)
    (cond
      [(atom? (car l)) (car l)]
      [else (leftmost (car l))])))

(check-equal? (leftmost '((a) (b  ((c) d) (b)))) 'a)
(check-equal? (leftmost '(((b) (c (d))) e)) 'b)

;; Determines if two lists of S-expressions are equal
(define eqlist?
  (lambda (l1 l2)
    (cond
      [(null? l1)
       (cond
         [(null? l2) #t]
         [else #f])]
      [(atom? (car l1))
       (cond
         [(atom? (car l2))
          (and (equan? (car l1) (car l2))
               (eqlist? (cdr l1) (cdr l2)))]
         [else #f])]
      [else
       (cond
         [(null? l2) #f]
         [(atom? (car l2)) #f]
         [else (and (eqlist? (car l1) (car l2))
                    (eqlist? (cdr l1) (cdr l2)))])])))

(check-equal? (eqlist? '(a b c) '(a b c)) #t)
(check-equal? (eqlist? '(a b c) '(a c b)) #f)
(check-equal? (eqlist? '(a ((b c) d)) '(a ((b c) d))) #t)
(check-equal? (eqlist? '(a ((b c) d)) '(a ((b d) c))) #f)