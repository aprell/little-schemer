#lang racket

(require rackunit)
(require "01.rkt")

(provide rember
         firsts
         insertR
         insertL
         subst
         subst2
         multirember
         multiinsertR
         multiinsertL
         multisubst)

;; Removes first occurrence of atom a from list of atoms lat
(define rember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) a) (cdr lat)]
      [else (cons (car lat) (rember a (cdr lat)))])))

(check-equal? (rember 'a '()) '())
(check-equal? (rember 'a '(b c)) '(b c))
(check-equal? (rember 'a '(b a c)) '(b c))
(check-equal? (rember 'a '(b a c a)) '(b c a))

;; Tail-recursive version of rember (CPS)
(define rember-tc
  (lambda (a lat cont)
    (cond
      [(null? lat) 
       (cont '())]
      [(eq? (car lat) a) 
       (cont (cdr lat))]
      [else
       (rember-tc a (cdr lat) 
         (lambda (newlat) (cont (cons (car lat) newlat))))])))

(define mycont (lambda (lat) lat))

(check-equal? (rember-tc 'a '() mycont) '())
(check-equal? (rember-tc 'a '(b c) mycont) '(b c))
(check-equal? (rember-tc 'a '(b a c) mycont) '(b c))
(check-equal? (rember-tc 'a '(b a c a) mycont) '(b c a))

;; Builds a new list composed of the first S-expression of each list in l 
(define firsts
  (lambda (l)
    (cond
      [(null? l) '()]
      [else (cons (car (car l)) (firsts (cdr l)))])))

(check-equal? (firsts '()) '())
(check-equal? (firsts '((a) (b c) (d (e f)))) '(a b d))
(check-equal? (firsts '(((a) b) ((c (d)) e) (((f g)) (h i)))) 
                      '((a) (c (d)) ((f g))))

;; Builds a new list with new inserted after the first occurrence of old
(define insertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old) (cons old (cons new (cdr lat)))]
      [else (cons (car lat) (insertR new old (cdr lat)))])))
      
(check-equal? (insertR 'b 'a '(a c)) '(a b c))
(check-equal? (insertR 'b 'a '(a c a)) '(a b c a))
(check-equal? (insertR 'c 'b '(a b c)) '(a b c c))
(check-equal? (insertR 'c 'b '(c d e f)) '(c d e f))

;; Builds a new list with new inserted before the first occurrence of old
(define insertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old) (cons new lat)]
      [else (cons (car lat) (insertL new old (cdr lat)))])))
      
(check-equal? (insertL 'b 'a '(a c)) '(b a c))
(check-equal? (insertL 'b 'a '(a c a)) '(b a c a))
(check-equal? (insertL 'c 'b '(a b c)) '(a c b c))
(check-equal? (insertL 'c 'b '(c d e f)) '(c d e f))

;; Builds a new list with the first occurrence of old replaced by new
(define subst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old) (cons new (cdr lat))]
      [else (cons (car lat) (subst new old (cdr lat)))])))
      
(check-equal? (subst 'b 'a '(a c)) '(b c))
(check-equal? (subst 'b 'a '(a c a)) '(b c a))
(check-equal? (subst 'c 'b '(a b c)) '(a c c))
(check-equal? (subst 'c 'b '(c d e f)) '(c d e f))

;; Variation of subst that replaces either the first occurrence of old1 or the
;; first occurrence of old2 by new
(define subst2
  (lambda (new old1 old2 lat)
    (cond
      [(null? lat) '()]
      [(or (eq? (car lat) old1) (eq? (car lat) old2)) (cons new (cdr lat))]
      [else (cons (car lat) (subst2 new old1 old2 (cdr lat)))])))
      
(check-equal? (subst2 'b 'a 'c '(a c)) '(b c))
(check-equal? (subst2 'b 'a 'd '(d c a)) '(b c a))
(check-equal? (subst2 'c 'b 'd '(a b c)) '(a c c))
(check-equal? (subst2 'c 'a 'b '(c d e f)) '(c d e f))

;; Removes all occurrences of a from lat
(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) a) (multirember a (cdr lat))]
      [else (cons (car lat) (multirember a (cdr lat)))])))

(check-equal? (multirember 'a '()) '())
(check-equal? (multirember 'a '(b c)) '(b c))
(check-equal? (multirember 'a '(b a c)) '(b c))
(check-equal? (multirember 'a '(b a c a)) '(b c))

;; Inserts new after every occurrence of old
(define multiinsertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old) 
            (cons old (cons new (multiinsertR new old (cdr lat))))]
      [else (cons (car lat) (multiinsertR new old (cdr lat)))])))
      
(check-equal? (multiinsertR 'b 'a '(a c)) '(a b c))
(check-equal? (multiinsertR 'b 'a '(a c a)) '(a b c a b))
(check-equal? (multiinsertR 'c 'b '(a b c)) '(a b c c))
(check-equal? (multiinsertR 'c 'b '(c d e f)) '(c d e f))

;; Inserts new before every occurrence of old
(define multiinsertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old) 
            (cons new (cons old (multiinsertL new old (cdr lat))))]
      [else (cons (car lat) (multiinsertL new old (cdr lat)))])))
      
(check-equal? (multiinsertL 'b 'a '(a c)) '(b a c))
(check-equal? (multiinsertL 'b 'a '(a c a)) '(b a c b a))
(check-equal? (multiinsertL 'c 'b '(a b c)) '(a c b c))
(check-equal? (multiinsertL 'c 'b '(c d e f)) '(c d e f))

;; Replaces all occurrences of old with new
(define multisubst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old) (cons new (multisubst new old (cdr lat)))]
      [else (cons (car lat) (multisubst new old (cdr lat)))])))
      
(check-equal? (multisubst 'b 'a '(a c)) '(b c))
(check-equal? (multisubst 'b 'a '(a c a)) '(b c b))
(check-equal? (multisubst 'c 'b '(a b c)) '(a c c))
(check-equal? (multisubst 'c 'b '(c d e f)) '(c d e f))
