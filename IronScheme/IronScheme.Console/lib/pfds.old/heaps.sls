#!r6rs
;; Copyright (C) 2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;; Documentation:
;;
;; make-heap : (any any -> bool) -> heap
;; returns a new empty heap which uses the ordering procedure.
;;
;; heap : (any any -> bool) any ... -> heap
;; return a new heap, ordered by the procedure argument, that contains
;; all the other arguments as elements.
;;
;; heap? : any -> bool
;; returns #t if the argument is a heap, #f otherwise.
;;
;; heap-size : heap -> non-negative integer
;; returns the number of elements in the heap.
;;
;; heap-empty? : heap -> bool
;; returns #t if the heap contains no elements, #f otherwise.
;;
;; heap-min : heap -> any
;; returns the minimum element in the heap, according the heap's
;; ordering procedure. If there are no elements, a
;; &heap-empty-condition is raised.
;;
;; heap-delete-min : heap -> heap
;; returns a new heap containing all the elements of the heap
;; argument, except for the minimum argument, as determined by the
;; heap's ordering procedure. If there are no elements, a
;; &heap-empty-condition is raised.
;;
;; heap-pop : any + heap
;; returns two values: the the minimum value, and a heap obtained by
;; removing the minimum value from the original heap. If the heap is
;; empty, a &heap-empty-condition is raised.
;;
;; heap-insert : heap any -> heap
;; returns the new heap obtained by adding the element to those in the
;; argument heap.
;;
;; heap->list : heap -> Listof(any)
;; returns the heap containing all the elements of the heap. The
;; elements of the list are ordered according to the heap's ordering
;; procedure.
;;
;; list->heap : Listof(any) (any any -> boolean) -> heap
;; returns the heap containing all the elements of the list, and using
;; the procedure argument to order the elements.
;;
;; heap-merge : heap heap -> heap
;; returns the heap containing all the elements of the argument
;; heaps. The argument heaps are assumed to be using the same ordering
;; procedure.
;;
;; heap-sort : (any any -> bool) list -> list
;; returns a new list that is a permutation of the argument list, such
;; that all the elements are ordered by the given procedure.
;;
;; heap-ordering-procedure : heap -> (any any -> boolean)
;; returns the ordering procedure used internally by the heap.
;;
;; heap-empty-condition? : any -> bool
;; returns #t if argument is a &heap-empty condition, #f otherwise.
;;
(library (pfds heaps)
(export make-heap
        (rename (%heap heap))
        heap?
        heap-size
        heap-empty?
        heap-min
        heap-delete-min
        heap-insert
        heap-pop
        heap->list
        list->heap
        heap-merge
        heap-sort
        (rename (heap-ordering-predicate heap-ordering-procedure))
        heap-empty-condition?
        )
(import (rnrs))

(define-record-type (node %make-node node?)
  (fields size height value left right))

(define-record-type leaf)

(define (height x)
  (if (leaf? x)
      0
      (node-height x)))

(define (size x)
  (if (leaf? x)
      0
      (node-size x)))

(define (make-node v l r)
  (define sl (height l))
  (define sr (height r))
  (define m (+ 1 (min sl sr)))
  (define sz (+ 1 (size l) (size r)))
  (if (< sl sr)
      (%make-node sz m v r l)
      (%make-node sz m v l r)))

(define (singleton v)
  (%make-node 1 0 v (make-leaf) (make-leaf)))

(define (insert tree value prio<?)
  (merge-trees tree (singleton value) prio<?))

(define (delete-min tree prio<?)
  (merge-trees (node-left tree)
               (node-right tree)
               prio<?))

(define (merge-trees tree1 tree2 prio<?)
  (cond ((leaf? tree1) tree2)
        ((leaf? tree2) tree1)
        ((prio<? (node-value tree2)
                 (node-value tree1))
         (make-node (node-value tree2)
                    (node-left tree2)
                    (merge-trees tree1
                                 (node-right tree2)
                                 prio<?)))
        (else
         (make-node (node-value tree1)
                    (node-left tree1)
                    (merge-trees (node-right tree1)
                                 tree2
                                 prio<?)))))


;; outside interface
(define-record-type (heap %make-heap heap?)
  (fields tree ordering-predicate))

(define (make-heap priority<?)
  (%make-heap (make-leaf) priority<?))

(define (%heap < . vals)
  (list->heap vals <))

(define (heap-size heap)
  (size (heap-tree heap)))

(define (heap-empty? heap)
  (leaf? (heap-tree heap)))

(define (heap-min heap)
  (when (heap-empty? heap)
    (raise (condition
            (make-heap-empty-condition)
            (make-who-condition 'heap-min)
            (make-message-condition "There is no minimum element.")
            (make-irritants-condition (list heap)))))
  (node-value (heap-tree heap)))

(define (heap-delete-min heap)
  (when (heap-empty? heap)
    (raise (condition
            (make-heap-empty-condition)
            (make-who-condition 'heap-delete-min)
            (make-message-condition "There is no minimum element.")
            (make-irritants-condition (list heap)))))
  (let ((< (heap-ordering-predicate heap)))
    (%make-heap (delete-min (heap-tree heap) <) <)))

(define (heap-pop heap)
  (when (heap-empty? heap)
    (raise (condition
            (make-heap-empty-condition)
            (make-who-condition 'heap-pop)
            (make-message-condition "There is no minimum element.")
            (make-irritants-condition (list heap)))))
  (let* ((tree (heap-tree heap))
         (top  (node-value tree))
         (<    (heap-ordering-predicate heap))
         (rest (delete-min tree <)))
    (values top
            (%make-heap rest <))))

(define (heap-insert heap value)
  (assert (heap? heap))
  (let ((< (heap-ordering-predicate heap)))
    (%make-heap (insert (heap-tree heap) value <) <)))

(define (heap->list heap)
  (assert (heap? heap))
  (let ((< (heap-ordering-predicate heap)))
    (let loop ((tree (heap-tree heap)) (list '()))
      (if (leaf? tree)
          (reverse list)
          (loop (delete-min tree <)
                (cons (node-value tree) list))))))

(define (list->heap list <)
  (%make-heap
   (fold-left (lambda (h item)
                (insert h item <))
              (make-leaf)
              list)
   <))

(define (heap-merge heap1 heap2)
  (define < (heap-ordering-predicate heap1))
  (%make-heap
   (merge-trees (heap-tree heap1)
                (heap-tree heap2)
                <)
   <))

(define (heap-sort < list)
  (heap->list (list->heap list <)))

(define-condition-type &heap-empty
  &assertion
  make-heap-empty-condition
  heap-empty-condition?)
)
