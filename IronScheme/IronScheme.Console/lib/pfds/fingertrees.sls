#!r6rs
;;; fingertrees.sls --- A Simple General-Purpose Data Structure

;; Copyright (C) 2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:
;;
;; Fingertrees are a generalised form of deque, that you can parameterise
;; to compute a value, called the "measure" of a fingertree. This measure
;; will be updated incrementally as you add and remove elements from the
;; fingertree. Among other things, this allows fingertrees to be used
;; where you otherwise might have written a custom data structure.
;;
;; To compute the measure, fingertrees require pieces of information: a
;; converter, a combiner, and an identity.
;;
;; The converter is a procedure of one argument, that maps values in the
;; fingertree to other values which are used for computing the measure.
;;
;; The combiner is a procedure of two arguments, and combines these into
;; one value representing them both. A combiner must be associative
;; i.e. (combine A (combine B C)) must be equivalent to (combine (combine
;; A B) C) for all values A, B and C.
;;
;; An identity is a value that represents the measure of an empty
;; fingertree. It must obey the rule that (combine X identity), (combine
;; identity X) and X are always the same.
;;
;; To make things more concrete, a simple use of a fingertree is as a
;; deque that keeps a running total. In this case, the converter can
;; simply be the function (lambda (x) x) if it is a deque of integers,
;; the combiner would be +, and the identity 0.
;;
;; (define l '(3 1 4 1 5 9))
;;
;; (define ft (list->fingertree l 0 + (lambda (x) x)))
;;
;; (fingertree-measure ft)
;; ; => 23
;; (fingertree-measure (fingertree-snoc ft 2))
;; ; => 25
;; (let-values (((head tail) (fingertree-uncons ft)))
;;   (fingertree-measure tail))
;; ; => 20
;;
;; Mathematically speaking, the _return type_ of the converter, the
;; combiner and the identity element are expected to form a
;; monoid.
;;
;; Below, I use the slightly incorrect terminology of referring to the
;; combiner, the converter, and the identity, together as a
;; monoid. Mathematicians, please forgive me. Programmers please forgive
;; me even more. If you can provide a better name (from a programmers,
;; not a mathematicians, point of view) that works in most circumstances,
;; I will be happy to use it.
;;
;; (FWIW the Haskell Data.Fingertree package uses odd name of Measured
;; (which are expected to be instances of Monoid))
;;
;; fingertree? : any -> bool
;; returns #t if argument is a fingertree, #f otherwise.
;;
;; fingertree-empty? : fingertree -> bool
;; returns #t if there are no items in the fingertree, #f otherwise.
;;
;; make-fingertree : id combine measure -> fingertree
;; returns a new fingertree, parameterised by the given monoid.
;;
;; fingertree-cons : any fingertree -> fingertree
;; returns the new fingertree created by adding the element to the front
;; of the argument fingertree.
;;
;; fingertree-snoc : fingertree any -> fingertree
;; returns the new fingertree created by adding the element to the end of
;; the fingertree.
;;
;; fingertree-uncons : fingertree -> any + fingertree
;; returns two values: the element at the front of the fingertree, and a
;; new fingertree containing all but the front element. If the fingertree
;; is empty, a &fingertree-empty condition is raised.
;;
;; fingertree-unsnoc : fingertree -> fingertree + any
;; returns two values: a new fingertree containing all but the rear
;; element of the argument fingertree, and the rear element itself. If
;; the fingertree is empty, a &fingertree-empty-condition is raised.
;;
;; fingertree-append : fingertree fingertree -> fingertree
;; returns a new fingertree which contains all of the elements of the
;; first fingertree argument, followed by all the elements of the
;; second. The argument fingertrees are assumed to be parameterised by
;; the same monoid.
;;
;; list->fingertree : (list->fingertree l id append convert)
;; returns a fingertree containing all of the elements of the argument
;; list, in the same order.
;;
;; fingertree->list : fingertree -> Listof(Any)
;; returns a list of all the elements in the fingertree, in the order
;; they would be unconsed.
;;
;; fingertree-measure : fingertree -> any
;; returns the measure of the fingertree, as defined by the fingertree's
;; monoid.
;;
;; fingertree-split : (any -> bool) fingertree -> fingertree + fingertree
;; returns two values: the first is the largest prefix of the fingertree for
;; which applying the predicate to it's accumulated measure returns
;; #f. The second values is a fingertree containing all those elements
;; not in the first fingertree.
;;
;; fingertree-split3: (any -> bool) fingertree -> fingertree + value + fingertree
;; similar to fingertree-split, however, instead of returning the
;; remainder as the second argument, it returns the head of the remainder
;; as the second argument, and tail of the remainder as the third
;; argument.
;; TODO: what error should I give if the remainder was empty?
;;
;; fingertree-fold : (any -> any -> any) any fingertree
;; returns the value obtained by iterating the combiner procedure over
;; the fingertree in left-to-right order. This procedure takes two
;; arguments, the current value from the fingertree, and an accumulator,
;; and it's return value is used as the accumulator for the next
;; iteration. The initial value for the accumulator is given by the base
;; argument.
;;
;; fingertree-fold-right : (any -> any -> any) any fingertree
;; similar to fingertree-fold, but iterates in right-to-left order.
;;
;; fingertree-reverse : fingertree -> fingertree
;; returns a new fingertree in which the elements are in the opposite
;; order from the argument fingertree.
;;
;; fingertree-empty-condition? : condition -> bool
;; returns #t if the argument is a &fingertree-empty condition, #f otherwise.
;;
(library (pfds fingertrees)
(export fingertree?
        fingertree-empty?
        make-fingertree
        fingertree-cons
        fingertree-snoc
        fingertree-uncons
        fingertree-unsnoc
        fingertree-append
        list->fingertree
        fingertree->list
        fingertree-measure
        fingertree-split
        fingertree-split3
        fingertree-fold
        fingertree-fold-right
        fingertree-reverse
        fingertree-empty-condition?
        )
(import (rnrs))

;;; List helpers

(define (snoc l val)
  (append l (list val)))

(define (take l n)
  (if (or (null? l) (zero? n))
      '()
      (cons (car l)
            (take (cdr l) (- n 1)))))

(define (last list)
  (if (null? (cdr list))
      (car list)
      (last (cdr list))))

(define (but-last list)
  (if (null? (cdr list))
      '()
      (cons (car list)
            (but-last (cdr list)))))

(define (map-reverse f l)
  (fold-left (lambda (o n) (cons (f n) o)) '() l))

;;; Node type

(define-record-type node2
  (protocol
   (lambda (new)
     (lambda (monoid a b)
       (define app (mappend monoid))
       (new (app (measure-nodetree a monoid)
                 (measure-nodetree b monoid))
            a
            b))))
  (fields measure a b))

(define-record-type node3
  (protocol
   (lambda (new)
     (lambda (monoid a b c)
       (define app (mappend monoid))
       (new (app (app (measure-nodetree a monoid)
                      (measure-nodetree b monoid))
                 (measure-nodetree c monoid))
            a
            b
            c))))
  (fields measure a b c))

(define (node-case node k2 k3)
  (if (node2? node)
      (k2 (node2-a node) (node2-b node))
      (k3 (node3-a node) (node3-b node) (node3-c node))))

(define (node-fold-right f base node)
  (node-case node
     (lambda (a b)
       (f a (f b base)))
     (lambda (a b c)
       (f a (f b (f c base))))))

(define (node->list node)
  (node-fold-right cons '() node))

(define (nodetree-fold-right f base nodetree)
  (define (foldr node base)
    (cond ((node2? node)
           (foldr (node2-a node)
                  (foldr (node2-b node) base)))
          ((node3? node)
           (foldr (node3-a node)
                  (foldr (node3-b node)
                         (foldr (node3-c node) base))))
          (else (f node base))))
  (foldr nodetree base))

(define (nodetree-fold-left f base nodetree)
  (define (foldl node base)
    (cond ((node2? node)
           (foldl (node2-b node)
                  (foldl (node2-a node) base)))
          ((node3? node)
           (foldl (node3-c node)
                  (foldl (node3-b node)
                         (foldl (node3-a node) base))))
          (else (f node base))))
  (foldl nodetree base))

;;; Tree type

(define-record-type empty)

(define-record-type single
  (fields value))

(define-record-type rib
  (protocol
   (lambda (new)
     (lambda (monoid left middle right)
       (define app (mappend monoid))
       (new (app (app (measure-digit left monoid)
                      (measure-ftree middle monoid))
                 (measure-digit right monoid))
            left
            middle
            right)
       )))
  ;; left and right expected to be lists of length 0 < l < 5
  (fields measure left middle right))

(define (ftree-case ftree empty-k single-k rib-k)
  (cond ((empty? ftree) (empty-k))
        ((single? ftree)
         (single-k (single-value ftree)))
        (else
         (rib-k (rib-left ftree)
                (rib-middle ftree)
                (rib-right ftree)))))

(define (digits-fold-right f b d)
  (fold-right (lambda (ntree base)
                (nodetree-fold-right f base ntree))
              b
              d))

(define (digits-fold-left f b d)
  (fold-left (lambda (base ntree)
                (nodetree-fold-left f base ntree))
              b
              d))

(define (ftree-fold-right proc base ftree)
  (ftree-case ftree
    (lambda () base)
    (lambda (x) (nodetree-fold-right proc base x))
    (lambda (l x r)
      (define base* (digits-fold-right proc base r))
      (define base** (ftree-fold-right proc base* x))
      (digits-fold-right proc base** l))))

(define (ftree-fold-left proc base ftree)
  (ftree-case ftree
    (lambda () base)
    (lambda (x) (nodetree-fold-left proc base x))
    (lambda (l x r)
      (define base* (digits-fold-left proc base l))
      (define base** (ftree-fold-left proc base* x))
      (digits-fold-left proc base** r))))

(define (insert-front ftree val monoid)
  (ftree-case ftree
    (lambda ()
      (make-single val))
    (lambda (a)
      (make-rib monoid (list val) (make-empty) (list a)))
    (lambda (l m r)
      (if (= (length l) 4)
          (make-rib monoid
                    (list val (car l))
                    (insert-front m (apply make-node3 monoid (cdr l)) monoid)
                    r)
          (make-rib monoid (cons val l) m r)))))

(define (view-front ftree empty-k cons-k monoid)
  (ftree-case ftree
              empty-k
              (lambda (a)
                (cons-k a (make-empty)))
              (lambda (l r m)
                (cons-k (car l)
                        (rib-l (cdr l) r m monoid)))))

(define (list->tree l monoid)
  (fold-right (lambda (val tree)
                (insert-front tree val monoid))
              (make-empty)
              l))

(define (rib-l l m r monoid)
  (if (null? l)
      (view-front m
        (lambda ()
          (list->tree r monoid))
        (lambda (x xs)
          (make-rib monoid
                    (node->list x)
                    xs
                    r))
        monoid)
      (make-rib monoid l m r)))

(define (remove-front ftree monoid)
  (view-front ftree
    (lambda ()
      (error 'remove-front "can't remove from an empty tree"))
    values
    monoid))

(define (insert-rear ftree val monoid)
  (ftree-case ftree
    (lambda ()
      (make-single val))
    (lambda (a)
      (make-rib monoid (list a) (make-empty) (list val)))
    (lambda (l m r)
      ;; TODO: should r be maintained in reverse order, rather than
      ;; normal?
      ;; yes! it will make concatenation slightly slower, but will
      ;; speed up inserts and removals
      (if (= (length r) 4)
          (make-rib monoid
                    l
                    (insert-rear m (apply make-node3 monoid (take r 3)) monoid)
                    (list (list-ref r 3) val))
          (make-rib monoid l m (snoc r val))))))

(define (remove-rear ftree monoid)
  (view-rear ftree
    (lambda ()
      (error 'remove-rear "can't remove from an empty tree"))
    values
    monoid))

(define (view-rear ftree empty-k snoc-k monoid)
  (ftree-case ftree
              empty-k
              (lambda (a)
                (snoc-k (make-empty) a))
              (lambda (l r m)
                (snoc-k (rib-r l r (but-last m) monoid)
                        (last m)))))

(define (rib-r l m r monoid)
  (if (null? r)
      (view-rear m
        (lambda ()
          (list->tree l monoid))
        (lambda (m* r*)
          (make-rib monoid l m* (node->list r*)))
        monoid)
      (make-rib monoid l m r)))

(define (insert-front/list tree l monoid)
  (fold-right (lambda (val tree)
                (insert-front tree val monoid))
              tree
              l))

(define (insert-rear/list tree l monoid)
  (fold-left (lambda (tree val)
                (insert-rear tree val monoid))
              tree
              l))

(define (app3 ftree1 ts ftree2 monoid)
  (cond ((empty? ftree1)
         (insert-front/list ftree2 ts monoid))
        ((empty? ftree2)
         (insert-rear/list ftree1 ts monoid))
        ((single? ftree1)
         (insert-front (insert-front/list ftree2 ts monoid)
                       (single-value ftree1)
                       monoid))
        ((single? ftree2)
         (insert-rear (insert-rear/list ftree1 ts monoid)
                      (single-value ftree2)
                      monoid))
        (else
         (let ((l1 (rib-left ftree1))
               (m1 (rib-middle ftree1))
               (r1 (rib-right ftree1))
               (l2 (rib-left ftree2))
               (m2 (rib-middle ftree2))
               (r2 (rib-right ftree2)))
           (make-rib monoid
                     l1
                     (app3 m1
                           (nodes (append r1 ts l2) monoid)
                           m2
                           monoid)
                     r2)))))

(define (nodes lst monoid)
  ;; *sigh*
  (let ((a (car lst))
        (b (cadr lst)))
    (cond ((null? (cddr lst))
           (list (make-node2 monoid a b)))
          ((null? (cdddr lst))
           (list (make-node3 monoid a b (caddr lst))))
          ((null? (cddddr lst))
           (list (make-node2 monoid a b)
                 (make-node2 monoid (caddr lst) (cadddr lst))))
          (else
           (cons (make-node3 monoid a b (caddr lst))
                 (nodes (cdddr lst) monoid))))))

(define (reverse-tree tree monoid)
  (ftree-case tree
    (lambda () (make-empty))
    (lambda (x) (make-single (reverse-nodetree x monoid)))
    (lambda (l x r)
      (make-rib monoid
                (reverse-digit r monoid)
                (reverse-tree x monoid)
                (reverse-digit l monoid)))))

(define (reverse-digit l monoid)
  (map-reverse (lambda (a) (reverse-nodetree a monoid)) l))

(define (reverse-nodetree l monoid)
  (cond ((node2? l)
         (make-node2 monoid
                     (reverse-nodetree (node2-b l) monoid)
                     (reverse-nodetree (node2-a l) monoid)))
        ((node3? l)
         (make-node3 monoid
                     (reverse-nodetree (node3-c l) monoid)
                     (reverse-nodetree (node3-b l) monoid)
                     (reverse-nodetree (node3-a l) monoid)))
        (else l)))

;; generalising fingertrees with monoids

;; I think I'm going to need a "configuration" type and pass it around
;; in order to generalize over arbitrary monoids
;; call the type iMeasured or something

(define-record-type monoid*
  ;; a monoid, but augmented with a procedure to convert objects into the
  ;; monoid type
  (fields (immutable empty mempty)
          (immutable append mappend)
          (immutable convert mconvert)))

(define (measure-digit obj monoid)
  (fold-left (lambda (i a)
               ((mappend monoid) i (measure-nodetree a monoid)))
             (mempty monoid)
             obj))

(define (measure-ftree obj monoid)
  (cond ((empty? obj)
         (mempty monoid))
        ((single? obj)
         (measure-nodetree (single-value obj) monoid))
        (else
         (rib-measure obj))))

(define (measure-nodetree obj monoid)
  (cond ((node2? obj) (node2-measure obj))
        ((node3? obj) (node3-measure obj))
        (else ((mconvert monoid) obj))))

(define (split proc tree monoid)
  (if (empty? tree)
      (values (make-empty) (make-empty))
      (if (proc (measure-ftree tree monoid))
          (let-values (((l x r) (split-tree proc (mempty monoid) tree monoid)))
            (values l (insert-front r x monoid)))
          (values tree (make-empty)))))

(define (split-tree proc i tree monoid)
  (ftree-case tree
    (lambda ()
      (error 'split-tree "shouldn't happen?"))
    (lambda (a)
      (values (make-empty) a (make-empty)))
    (lambda (l m r)
      (define app (mappend monoid))
      (define vpr (app i (measure-digit l monoid)))
      (define vm  (app vpr (measure-ftree m monoid)))
      (cond ((proc vpr)
             (let-values (((l* x* r*) (split-digit proc i l monoid)))
               (values (list->tree l* monoid)
                       x*
                       (rib-l r* m r monoid))))
            ((proc vm)
             (let*-values (((ml xs mr) (split-tree proc vpr m monoid))
                           ((l* x* r*)
                            (split-digit proc
                                         (app vpr (measure-ftree ml monoid))
                                         (node->list xs)
                                         monoid)))
               (values (rib-r l ml l* monoid)
                       x*
                       (rib-l r* mr r monoid))))
            (else
             (let-values (((l* x* r*) (split-digit proc vm r monoid)))
               (values (rib-r l m l* monoid)
                       x*
                       (list->tree r* monoid))))))))

(define (split-digit proc i xs monoid)
  (if (null? (cdr xs))
      (values '() (car xs) '())
      (let ((i* ((mappend monoid) i (measure-nodetree (car xs) monoid))))
        (if (proc i*)
            (values '() (car xs) (cdr xs))
            (let-values (((l x r)
                          (split-digit proc i* (cdr xs) monoid)))
              (values (cons (car xs) l) x r))))))

;; exported interface
(define-condition-type &fingertree-empty
  &assertion
  make-fingertree-empty-condition
  fingertree-empty-condition?)

(define-record-type (fingertree %make-fingertree fingertree?)
  (fields tree monoid))

(define (%wrap fingertree tree)
  (%make-fingertree tree
                    (fingertree-monoid fingertree)))

(define (make-fingertree id append convert)
  (%make-fingertree (make-empty)
                    (make-monoid* id append convert)))

(define (fingertree-cons a fingertree)
  ;; TODO: should it obey normal cons interface, or have fingertree
  ;; first?
  (%wrap fingertree
         (insert-front (fingertree-tree fingertree)
                       a
                       (fingertree-monoid fingertree))))

(define (fingertree-snoc fingertree a)
  (%wrap fingertree
         (insert-rear (fingertree-tree fingertree)
                      a
                      (fingertree-monoid fingertree))))

(define (fingertree-uncons fingertree)
  (call-with-values
      (lambda ()
        (define t (fingertree-tree fingertree))
        (when (empty? t)
          (raise
           (condition
            (make-fingertree-empty-condition)
            (make-who-condition 'fingertree-uncons)
            (make-message-condition "There are no elements to uncons")
            (make-irritants-condition (list fingertree)))))
        (remove-front t (fingertree-monoid fingertree)))
    (lambda (val rest)
      (values val
              (%wrap fingertree rest)))))

(define (fingertree-unsnoc fingertree)
  (call-with-values
      (lambda ()
        (define t (fingertree-tree fingertree))
        (when (empty? t)
          (raise
           (condition
            (make-fingertree-empty-condition)
            (make-who-condition 'fingertree-unsnoc)
            (make-message-condition "There are no elements to unsnoc")
            (make-irritants-condition (list fingertree)))))
        (remove-rear t (fingertree-monoid fingertree)))
    (lambda (rest val)
      (values (%wrap fingertree rest) val))))

(define (fingertree-empty? fingertree)
  (empty? (fingertree-tree fingertree)))

(define (fingertree-append fingertree1 fingertree2)
  (%wrap fingertree1
         (app3 (fingertree-tree fingertree1)
               '()
               (fingertree-tree fingertree2)
               (fingertree-monoid fingertree1))))

;; TODO: fix this
(define (list->fingertree l id append convert)
  (define monoid (make-monoid* id append convert))
  (%make-fingertree (list->tree l monoid) monoid))

(define (fingertree->list t)
  (fingertree-fold-right cons '() t))

(define (fingertree-measure fingertree)
  (measure-ftree (fingertree-tree fingertree)
                 (fingertree-monoid fingertree)))


(define (fingertree-split p fingertree)
  (call-with-values
      (lambda ()
        (split p
               (fingertree-tree fingertree)
               (fingertree-monoid fingertree)))
    (lambda (a b)
      (values (%wrap fingertree a)
              (%wrap fingertree b)))))

(define (fingertree-split3 p fingertree)
  (call-with-values
      (lambda ()
        (define monoid (fingertree-monoid fingertree))
        (split-tree p
                    (mempty monoid)
                    (fingertree-tree fingertree)
                    monoid))
    (lambda (a b c)
      (values (%wrap fingertree a)
              b
              (%wrap fingertree c)))))

(define (fingertree-fold f b fingertree)
  (ftree-fold-left f b (fingertree-tree fingertree)))

(define (fingertree-fold-right f b fingertree)
  (ftree-fold-right f b (fingertree-tree fingertree)))

(define (fingertree-reverse fingertree)
  (%wrap fingertree
         (reverse-tree (fingertree-tree fingertree)
                       (fingertree-monoid fingertree))))

)
