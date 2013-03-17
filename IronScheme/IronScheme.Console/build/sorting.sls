#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme sorting)
  (export
    list-sort
    vector-sort
    vector-sort!)
  (import 
    (except (rnrs) list-sort vector-sort vector-sort!)
    (ironscheme unsafe)
    (ironscheme contracts)
    (ironscheme clr))
  
  (define (split ls)
    (let loop 
        ((rest ls)
	       (left '())
	       (right '()))
       (cond 
         ((null? rest) (cons left right))
         ((null? (cdr rest)) (cons (cons (car rest) left) right))
         (else (loop 
                  (cddr rest)
                  (cons (car rest) left)
                  (cons (cadr rest) right))))))

  (define (reverse-it head tail)
    (if (null? head)
        tail
        (reverse-it 
          (cdr head) 
          (cons (car head) tail))))
         
  (define (merge list-1 list-2 precedes?)
    (let loop 
        ((source-1 list-1)
         (source-2 list-2)
         (so-far '()))
       (cond 
        ((null? source-1)
          (reverse-it so-far source-2))
        ((null? source-2)
           (reverse-it so-far source-1))
        (else
          (let ((car-1 (car source-1))
                (car-2 (car source-2)))
            (if (precedes? car-2 car-1)
                (loop source-1
                  (cdr source-2)
                  (cons car-2 so-far))
                (loop source-2
                  (cdr source-1)
                  (cons car-1 so-far))))))))         

  (define/contract (list-sort precedes?:procedure ls:list)
    (if (null? ls)
        '()
         (let helper ((piece ls))
          (if (null? (cdr piece))
              piece
              (let ((parts (split piece)))
                (merge (helper (car parts))
                       (helper (cdr parts)) 
                       precedes?))))))
                       
  (define/contract (vector-sort! pred?:procedure vec:vector)
    (clr-guard (e [e (assertion-violation 'vector-sort! (clr-prop-get Exception Message e) pred? vec)])
      (clr-static-call Array 
                       (Sort #(Object) Object[] (Comparison Object))
                       vec                      
                       (lambda (a b)
                         (if (eq? a b) 
                             0
                             (if (pred? a b)
                                 -1
                                 1))))))

  (define/contract (vector-sort pred?:procedure vec:vector)
    (let ((vec (clr-call Array Clone vec)))
      (vector-sort! pred? vec)
      vec)))
