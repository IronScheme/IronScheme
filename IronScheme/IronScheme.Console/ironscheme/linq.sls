#!r6rs

#| License
Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

#|

LINQ for R6RS Scheme
====================

See below code for examples and tests.

To run on ikarus, modify the 'get-hashtable' procedure.
To run on larceny the file needs to have a .sls extension.

Runs on IronScheme and MzScheme 3.99.0.23 as well.

The following grammar is as per the C# spec [1]. 

Grammar notes:
- The optional type token is not present and has been removed from the grammar.
- IDENTIFIER is any Scheme identifier.
- Other uppercased or single quoted 'tokens' are translated to lowercased Scheme syntax, with exceptions.
- expression is any Scheme expression.
- boolean_expression is Scheme expression returning a boolean value.
- * denotes 0 or more.
- ? denotes optional.
- Differences in C++ style comments.
- Starts with query_expression.

query_expression
  : from_clause query_body
  ;
  
from_clause
  : FROM IDENTIFIER IN expression
  ;
  
query_body
  : query_body_clause* select_or_group_clause query_continuation?
  ;
  
query_body_clause
  : from_clause
  | let_clause
  | where_clause
  | join_clause
  | join_into_clause
  | orderby_clause
  ;
  
let_clause
  : LET IDENTIFIER '=' expression
  ;
  
where_clause
  : WHERE boolean_expression
  ;
  
join_clause
  : JOIN IDENTIFIER IN expression ON expression EQUALS expression 
  ;
  
join_into_clause
  : JOIN IDENTIFIER IN expression ON expression EQUALS expression INTO IDENTIFIER
  ;
  
orderby_clause
  : ORDERBY orderings
  ;

// ',' is replaced by THEN  
orderings
  : ordering
  | orderings ',' ordering
  ;
  
ordering
  : expression ordering_direction?
  ;

ordering_direction
  : ASCENDING
  | DESCENDING
  ;
  
select_or_group_clause
  : select_clause
  | group_clause
  ;
  
select_clause
  : SELECT expression
  ;
  
group_clause
  : GROUP expression BY expression
  ;

query_continuation
  : INTO IDENTIFIER query_body 
  ;
  
References:

1. http://download.microsoft.com/download/3/8/8/388e7205-bc10-4226-b2a8-75351c669b09/CSharp%20Language%20Specification.doc , 7.15 Query expressions

|#

(library (ironscheme linq)
  (export
    grouping? ; determines if the 'comprehension' is a grouping
    get-list  ; returns the values of a grouping or just returns the list
    foreach   ; C# like syntactic sugar, supports 'break' exit proc
    key       ; gets the key of a grouping
    ; auxiliary keywords
    in
    select
    orderby
    where
    join
    group
    by
    equals
    on
    ascending
    descending
    let
    =
    then
    ; helper procs from LINQ
    take
    skip
    first
    first/default
    single
    single/default
    ; main macro
    from)
  (import 
    (rnrs))

  (define-record-type grouping (fields key values))
  
  (define-syntax define-aux
    (syntax-rules ()
      [(_ id ...)
        (begin
          (define-syntax id
            (lambda (x)
              (syntax-violation #f "invalid use of auxiliary keyword" x 'id))) ...)]))
            
  (define-aux 
    select
    in
    orderby
    where
    join
    group
    by
    equals
    on
    ascending
    descending
    then)
  
  (define (take lst n)
    (let ((lst (get-list lst)))
      (let f ((i 0)(lst lst)(a '()))
        (if (or (null? lst) (>= i n))
          (reverse a)
          (f (+ i 1) (cdr lst) (cons (car lst) a))))))
          
  (define (skip lst n)
    (let ((lst (get-list lst)))
      (let f ((i 0)(lst lst))
        (if (or (null? lst) (>= i n))
          lst
          (f (+ i 1) (cdr lst))))))
          
  (define (single lst)
    (let ((lst (get-list lst)))
      (if (= (length lst) 1)
        (car lst)
        (assertion-violation 'single "list does not contain 1 element" lst))))

  (define (single/default lst default)
    (let ((lst (get-list lst)))
      (case (length lst)
        [(0)  default]
        [(1)  (car lst)]
        [else
          (assertion-violation 'single/default "list contains more than 1 element" lst)])))
          
  (define (first lst)
    (let ((lst (get-list lst)))
      (if (null? lst)
        (assertion-violation 'first "list is empty" lst)
        (car lst))))

  (define (first/default lst default)
    (let ((lst (get-list lst)))
      (if (null? lst)
        default
        (car lst))))

  (define (get-list obj)
    (cond
      [(null? obj)     '()]
      [(pair? obj)     obj]
      [(grouping? obj) (grouping-values obj)]
      [else
        (assertion-violation 'get-list "not supported" obj)]))

  (define key grouping-key)

  (define-syntax foreach
    (lambda (x)
      (syntax-case x (in)
        [(foreach e in lst body body* ...)
          (with-syntax
            ([break (datum->syntax #'foreach 'break)])
              #'(call/cc
                  (lambda (break)
                    (for-each
                      (lambda (e) body body* ...)
                      (get-list lst)))))])))

  (define (identity x) x)

  (define (map* proc lst)
    (if (eq? proc identity) 
      (get-list lst)
      (map proc (get-list lst))))

  (define (filter* proc lst)
    (filter proc (get-list lst)))

  (define (get-eq a)
    (cond
      [(string? a) string=?]
      [(number? a) =]
      [(char? a)   char=?]
      [else        eqv?]))

  (define (get-hashtable e)
    ;(make-hashtable equal-hash (get-eq e)))
    (make-eqv-hashtable)) ; for ikarus

  (define (symbol<? a b)
    (string<? 
      (symbol->string a) 
      (symbol->string b)))

  (define (symbol>? a b)
    (string>? 
      (symbol->string a) 
      (symbol->string b)))

  (define (get-comparer asc? a)
    (cond
      [(number? a) (if asc? < >)]
      [(char? a)   (if asc? char<? char>?)]
      [(string? a) (if asc? string<? string>?)]
      [(symbol? a) (if asc? symbol<? symbol>?)]
      [else
        (assertion-violation 'get-comparer "not supported" a)]))

  (define-record-type sorter
    (fields
      asc?
      sel
      (mutable cmp)
      (mutable eq))
    (protocol
      (lambda (p)
        (lambda (asc? sel)
          (p asc? sel #f #f)))))

  (define (sort l . sorters)
    (let ((l (get-list l)))
      (if (null? l) '()
        (let ((r (cdr l)))
          (if (null? r) l
            (list-sort
              (lambda (a b)
                (let f ((sorters sorters))
                  (if (null? sorters) #f
                    (let* ((s (car sorters))
                           (sel (sorter-sel s))
                           (a (sel a))
                           (b (sel b)))
                      (unless (sorter-eq s)
                        (sorter-eq-set! s (get-eq a))
                        (sorter-cmp-set! s 
                          (get-comparer (sorter-asc? s) a)))
                      (if ((sorter-eq s) a b)
                        (f (cdr sorters))
                        ((sorter-cmp s) a b))))))
              l))))))

  (define (group-by sel proc lst)
    (let ((l (get-list lst)))
      (if (null? l) '()
        (let ((ht (get-hashtable (car l))))
          (for-each
            (lambda (x)
              (let ((key (proc x)))
                (hashtable-update! ht key
                  (lambda (o)
                    (cons (sel x) o))
                  '())))
            l)
          (vector->list
            (vector-map
              (lambda (key)
                (make-grouping key (reverse (hashtable-ref ht key '()))))
              (hashtable-keys ht)))))))

  (define (flatten lst)
    (apply append lst))

  ; this is a bit daft, but conforms with the output on LINQ in .NET
  (define (group-if-needed key vals)
    (if (null? vals) '()
      (make-grouping key vals)))

  (define-syntax bind*
    (syntax-rules ()
      [(bind* (var) K body)
        (lambda (var)
          (let ((K (list var)))
            body))]
      [(bind* (vars ...) K body)
        (lambda (K)
          (apply
            (lambda (vars ...) body)
            K))]))
            
  (define-syntax bind
    (lambda (x)
      (syntax-case x ()
        [(bind (var) body)
          (and 
            (identifier? #'body)
            (bound-identifier=? #'var #'body))
          #'identity]
        [(bind (var) body)
          #'(lambda (var) body)]
        [(bind (vars ...) body)
          #'(lambda (K)
            (apply
              (lambda (vars ...) body)
              K))])))

  (define-syntax from
    (lambda (x)
      (define (valid-id? e vars)
        (and (identifier? e)
          (not (memp
                (lambda (x)
                  (bound-identifier=? e x))
                vars))))
      (syntax-case x (in)
        [(_ e in l rest ...)
          (identifier? #'e)
          (let recur ((vars (list #'e))
                      (l #'l)
                      (x* #'(rest ...)))
            (syntax-case x* (where let = in select from orderby
                             join equals on group by into)
              [() (syntax-violation 'from "missing select or group by clause" x)]
              [(select s into k rest ...)
                #`(let* ((k (map* (bind #,vars s) #,l)))
                  #,(recur (list #'k) #'k #'(rest ...)))]
              [(select s)
                #`(map* (bind #,vars s) #,l)]
              [(group g by b into k rest ...)
                #`(let* ((k (group-by (bind #,vars g) (bind #,vars b) #,l)))
                  #,(recur (list #'k) #'k #'(rest ...)))]
              [(group g by b)
                #`(group-by (bind #,vars g) (bind #,vars b) #,l)]
              [(where p rest ...)
                (recur vars
                  #`(filter* (bind #,vars p) #,l)
                  #'(rest ...))]
              [(from e* in l* rest ...)
                (if (valid-id? #'e* vars)
                  (recur (cons #'e* vars)
                    #`(flatten
                        (map*
                          (bind* #,vars K
                            (map* (lambda (K*) (cons K* K)) l*))
                          #,l))
                     #'(rest ...))
                  (syntax-violation 'from "not a unique identifier" #'e* x*))]
              [(let a = b rest ...)
                (if (valid-id? #'a vars)
                  (recur (cons #'a vars)
                    #`(map* (bind* #,vars K (cons b K)) #,l)
                    #'(rest ...))
                  (syntax-violation 'from "not a unique identifier" #'a x*))]
              [(orderby rest ...)
                (with-syntax ((asc #'ascending)
                              (desc #'descending))
                  (let f ((x #'(rest ...))
                          (se '()))
                    (syntax-case x (then)
                      [(p dir then rest ...)
                        (or (free-identifier=? #'dir #'asc)
                            (free-identifier=? #'dir #'desc))
                        (f #'(rest ...)
                          (cons
                            #`(make-sorter
                                #,(free-identifier=? #'dir #'asc)
                                (bind #,vars p))
                             se))]
                      [(p dir rest ...)
                        (or (free-identifier=? #'dir #'asc)
                            (free-identifier=? #'dir #'desc))
                        (recur vars
                          #`(sort #,l
                              #,@(reverse se)
                              (make-sorter
                                #,(free-identifier=? #'dir #'asc)
                                (bind #,vars p)))
                          #'(rest ...))]
                      [(p rest ...)
                        (f #'(p asc rest ...) se)])))]
              [(join e* in l* on a equals b into f rest ...)
                (if (valid-id? #'e* vars)
                  (if (valid-id? #'f* vars)
                    (with-syntax (((a* eq) (generate-temporaries '(a* eq))))
                      (recur vars l
                         #'(let a* = a
                            let eq = (get-eq a*)
                            let f = (group-if-needed a*
                             (from e* in l*
                              where (eq a* b)
                              select e*)) rest ...)))
                    (syntax-violation 'from "not a unique identifier" #'f x*))
                  (syntax-violation 'from "not a unique identifier" #'e* x*))]
              [(join e* in l* on a equals b rest ...)
                (if (valid-id? #'e* vars)
                  (with-syntax (((a* eq) (generate-temporaries '(a* eq))))
                    (recur vars l
                      #'(let a* = a
                         let eq = (get-eq a*)
                         from e* in l* 
                         where (eq a* b) 
                         rest ...)))
                  (syntax-violation 'from "not a unique identifier" #'e* x*))]
              ))])))

          
)

#|

Examples:
=========

(import (ironscheme linq))
; expects a library with printf too

(define gl '((a 4) (d 3) (a 2) (c 9) (a 11) (b 4) (b 1) (b 3)))
(define xl '((a 4 3) (d 3 6) (a 4 6) (c 9 5) (a 11 4) (b 4 3) (b 1 3) (b 3 2)
             (a 4 4) (d 3 1) (a 4 5) (c 9 6) (a 11 3) (b 4 1) (b 1 4) (b 3 9)))

(write
  ( from x in xl
    let a = (car x)
    let b = (cadr x)
    let c = (caddr x)
    orderby a ascending
       then c ascending
       then b descending
    where (and (even? (* b c)) (> b c))
    select x))
(newline)

(write
  ( from x in xl
    let a = (car x)
    let b = (cadr x)
    let c = (caddr x)
    where (and (even? (* b c)) (> b c))
    orderby a ascending
       then c ascending
       then b descending
    select x))
(newline)

(foreach x in
  (from y in gl
   group (cadr y) by (car y) into g
   orderby (key g)
   select g)
  (printf "~a =>\n" (key x))
  (foreach y in
    (from y in x
     orderby y descending
     select y)
    (printf "     ~a\n" y)))

(foreach x in
  (from y in gl
   let a = (cadr y)
   orderby a descending
   group a by (car y) into g
   orderby (key g)
   select g)
  (printf "~a =>\n" (key x))
  (foreach y in x
    (printf "     ~a\n" y)))

(write
(
  from y in '((a 4) (d 3) (a 2) (c 9) (a 11))
  group (cadr y) by (car y)

))
(newline)

(write
(
  from x in gl
  select x into f
  from y in f
  select y
  ))
(newline)

(define args (list (string->list "hello") (string->list "world")))

(display
  [
    from K in args
    from K* in K
    where (not (char=? #\l K*))
    orderby K*
    select K*
  ])
(newline) ;=> (d e h o o r w)


(write
  [
from y in '((a 4) (d 3) (a 2) (c 9) (a 11))
let a = (car y)  ; the symbol
let b = (cadr y) ; the number
let c = y
orderby b
where (and (eq? a 'a) (even? b))
from x in c
let d = x
select d
  ])
(newline) ;=> (a 2 a 4)


; Project Euler problem 1 and 2

(define (range s e)
  (let f ((e (- e 1))(o '()))
    (if (= s e) 
      (cons e o)
      (f (- e 1) (cons e o)))))
        
        
(define pe1 (delay 
             ( from x in (range 0 100000)
               where (or (= (mod x 5) 0) (= (mod x 3) 0))
               select x)))
               
             
              
(write
  (apply + (force pe1)))
(newline)  

(define (fib-seq till)
  (let f ((a 1)(b 2)(l '(2 1)))
    (let ((c (+ a b)))
      (if (< c till)
        (f b c (cons c l))
        l))))
        
(define pe2 ( from x in (fib-seq 4000000)
              where (even? x)
              select x))       
        
(write
  (apply + pe2))
(newline)  

Tests:
======

(import (ironscheme linq))

;; conformance to C# tests
;; simply permutations of the grammar and matched to the output of C#
(define (print-list lst)
  (foreach x in lst
    (printf "~a, " x))
  (printf "\n"))

(define selectdata  '(1 5 3 4 2))
(define groupdata   '(2 5 2 4 2))
(define nestdata    '((2 5)(2 4)(3 5)(3 1)(1 1)))


(define a ( from x in selectdata
            select x))

(print-list a)

(define a2 (from x in (from y in selectdata
                       select (+ y 1))
            select (- x 1)))

(print-list a2)

(define b ( from x in selectdata
            where (even? x)
            select x))

(print-list b)

(define c ( from x in selectdata
            orderby x
            select x))

(print-list c)

(define d ( from x in selectdata
            orderby x descending
            select x))

(print-list d)

(define e ( from x in selectdata
            where (odd? x)
            orderby x
            select x))

(print-list e)

(define f ( from x in selectdata
            let y = (* x x)
            select y))

(print-list f)

(define f2 (from x in selectdata
            let y = (* x x)
            where (odd? y)
            orderby y descending
            select y))

(print-list f2)

(define g ( from x in selectdata
            select x into z
            select z))

(print-list g)

(define h ( from x in nestdata
            from y in x
            select y))

(print-list h)

(define i ( from x in nestdata
            where (= (car x) 2)
            from y in x
            select y))

(print-list i)

(define j ( from x in nestdata
            orderby (car x)
            from y in x
            select y))

(print-list j)

(define k ( from x in nestdata
            from y in x
            orderby y
            select y))

(print-list k)

(define l ( from x in nestdata
            select x into y
            from z in y
            select z))

(print-list l)

(define m ( from x in nestdata
            group (car x) by (cadr x)))

(print-list m)

(define n ( from x in nestdata
            group (cadr x) by (car x)))

(print-list n)

(define o ( from x in nestdata
            group (cadr x) by (car x) into z
            select z))

(print-list o)

(define p ( from x in nestdata
            group (cadr x) by (car x) into z
            orderby (key z) descending
            select z))

(print-list p)

(define q ( from x in selectdata
            join y in groupdata on x equals y into z
            from w in z
            select w))

(print-list q)

(define r ( from x in selectdata
            join y in groupdata on x equals y
            select (cons x y)))

(print-list r)

(define s ( from x in selectdata
            from y in groupdata
            where (and (= x 4) (= y 2))
            select (cons x y)))

(print-list s)

(define t ( from x in selectdata
            join y in groupdata on x equals y into z
            orderby x
            select z))

(print-list t)

(define u ( from x in selectdata
            join y in groupdata on x equals y into z
            orderby x
            select (cons x z)))

(print-list u)

(define v ( from x in selectdata
            from y in groupdata select y into z
            where (even? z)
            select z))

(print-list v)


|#
