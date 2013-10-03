#lang racket

(require rackunit)
(require "01.rkt"
         "07.rkt")

(provide value)

;; Looks up the value of name in entry
;; If name is not found, entry-f is invoked 
;; An entry is a pair of lists of equal length
;; The first list must be a set
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      [(null? names) (entry-f name)]
      [(eq? (car names) name) (car values)]
      [else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)])))

(check-equal? (lookup-in-entry 'a '((a b c) (1 2 3)) #f) 1)
(check-equal? (lookup-in-entry 'b '((a b c) (x y z)) #f) 'y)
(check-equal? (lookup-in-entry 'c '((a b c) ((1) (2 3) (4 5))) #f) '(4 5))

;; Looks up the value of name in table
;; If name is not found, table-f is invoked
;; A table (also called an environment) is a list of entries
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      [(null? table) (table-f name)]
      [else 
       (lookup-in-entry name (car table)
         (lambda (name)
           (lookup-in-table name (cdr table) table-f)))])))

(define table1 '(((a b) (2 3))))
(define table2 '(((a b c) (1 2 3)) ((c d) (4 5))))
(define table3 '(((a b c) (x y z)) ((c d) (1 2)) ((e f) ((3 4) (7)))))

(check-equal? (lookup-in-table 'a table1 #f) 2)
(check-equal? (lookup-in-table 'c table2 #f) 3)
(check-equal? (lookup-in-table 'e table3 #f) '(3 4))

;; Finds out the type of an expression and returns the appropriate action 
(define expression-to-action
  (lambda (e)
    (cond
      [(atom? e) (atom-to-action e)]
      [else (list-to-action e)])))

;; Helper function for expressions that are atoms
(define atom-to-action
  (lambda (e)
    (cond
      [(number? e)      *const]
      [(eq? e #t)       *const]
      [(eq? e #f)       *const]
      [(eq? e 'cons)    *const]
      [(eq? e 'car)     *const]
      [(eq? e 'cdr)     *const]
      [(eq? e 'null?)   *const]
      [(eq? e 'eq?)     *const]
      [(eq? e 'atom?)   *const]
      [(eq? e 'zero?)   *const]
      [(eq? e 'add1)    *const]
      [(eq? e 'sub1)    *const]
      [(eq? e 'number?) *const]
      [else *identifier])))

;; Helper function for expressions that are lists
(define list-to-action
  (lambda (e)
    (cond
      [(atom? (car e))
       (cond
         [(eq? (car e) 'quote)  *quote]
         [(eq? (car e) 'lambda) *lambda]
         [(eq? (car e) 'cond)   *cond]
         [else *application])]
      [else *application])))

;; Determines the value of an expression (evaluates an expression)
(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))
    
;; Action for constants
(define *const
  (lambda (e table)
    (cond
      [(number? e) e]
      [(eq? e #t) #t]
      [(eq? e #f) #f]
      [else (build 'primitive e)])))

(check-equal? (*const 2 table1) 2)
(check-equal? (*const #t table1) #t)
(check-equal? (*const #f table1) #f)
(check-equal? (*const 'car table1) '(primitive car))
(check-equal? (*const 'cdr table1) '(primitive cdr))

;; Action for identifiers
;; Look up value in table
(define *identifier
  (lambda (e table)
    (lookup-in-table e table value-not-found)))

(define value-not-found exit)

(check-equal? (*identifier 'a table1) 2)
(check-equal? (*identifier 'b table1) 3)
(check-equal? (*identifier 'c table2) 3)

;; Action for quotations
(define *quote
  (lambda (e table)
    (second e)))
  
(check-equal? (*quote '(quote Hello) table1) 'Hello)
(check-equal? (*quote '(quote World!) table1) 'World!)

;; Action for lambdas (non-primitive functions)
(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))

(check-equal? (*lambda '(lambda (x) x) table1) 
              '(non-primitive ((((a b) (2 3))) (x) x)))
(check-equal? (*lambda '(lambda (x) (cons x y)) table2)
              '(non-primitive ((((a b c) (1 2 3)) ((c d) (4 5))) 
                               (x) (cons x y))))

(define table-of first)
(define formals-of second)
(define body-of third)

;; Action for conds
;; Evaluate cond-expression
(define *cond
  (lambda (e table)
    (eval-cond (cdr e) table)))

(define eval-cond
  (lambda (lines table)
    (cond
      [(else? (first (car lines)))
       (meaning (second (car lines)) table)]
      [(meaning (first (car lines)) table)
       (meaning (second (car lines)) table)]
      [else (eval-cond (cdr lines) table)])))

(define else?
  (lambda (x)
    (cond
      [(atom? x) (eq? x 'else)]
      [else #f])))

(define table4 '(((a b) (#t #f)) ((c d) (1 2))))
(define cond1 '(cond ((a b) c) (else d)))
(define cond2 '(cond ((b a) c) (else d)))
(define cond3 '(cond ((eq? a 2) b) (else 0)))

(check-equal? (*cond cond1 table4) 1)
(check-equal? (*cond cond2 table4) 2)

;; Action for function applications
(define *application
  (lambda (e table)
    (apply (meaning (car e) table) (eval-list (cdr e) table))))

(define eval-list
  (lambda (args table)
    (cond
      [(null? args) '()]
      [else (cons (meaning (car args) table)
                  (eval-list (cdr args) table))])))

(check-equal? (eval-list '(a b) table1) '(2 3))
(check-equal? (eval-list '(c d) table2) '(3 5))
(check-equal? (eval-list '(c d e) table3) '(z 2 (3 4)))
(check-equal? (eval-list '(a c b d) table4) '(#t 1 #f 2))

;; Functions are either primitive or non-primitive (closures)
(define primitive?
  (lambda (l)
    (eq? (car l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (car l) 'non-primitive)))

(define apply
  (lambda (fun vals)
    (cond
      [(primitive? fun) (apply-primitive (second fun) vals)]
      [(non-primitive? fun) (apply-closure (second fun) vals)])))

(define apply-primitive
  (lambda (name vals)
    (cond
      [(eq? name 'cons)    (cons (first vals) (second vals))]
      [(eq? name 'car)     (car (first vals))]
      [(eq? name 'cdr)     (cdr (first vals))]
      [(eq? name 'null?)   (null? (first vals))]
      [(eq? name 'eq?)     (eq? (first vals) (second vals))]
      [(eq? name 'atom?)   (atom? (first vals))]
      [(eq? name 'zero?)   (zero? (first vals))]
      [(eq? name 'add1)    (add1 (first vals))]
      [(eq? name 'sub1)    (sub1 (first vals))]
      [(eq? name 'number?) (number? (first vals))])))
      
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
               (new-entry (formals-of closure) vals)
               (table-of closure)))))

(define extend-table cons)
(define new-entry build)
