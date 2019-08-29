#!r6rs
;;; sequences.sls --- Purely Functional Sequences

;; Copyright (C) 2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; Sequences are a general-purpose, variable-length collection,
;; similar to lists, however they support efficient addition and
;; removal from both ends, and random-access. Like other Scheme
;; collections, sequences are zero-indexed.
;;        
;; make-sequence : () -> sequence
;; returns a new empty sequence
;;
;; sequence any ... -> sequence
;; returns a new sequence containing all of the argument elements, in the
;; same order.
;;
;; sequence? : any -> bool
;; returns #t if the argument is a sequence, #f otherwise.
;;
;; sequence-empty? : sequence -> bool
;; returns #t if the argument sequence contains no elements, #f otherwise.
;;
;; sequence-size : sequence -> non-negative integer
;; returns the number of elements in the sequence
;;
;; sequence-cons : any sequence -> sequence
;; return the new sequence created by adding the element to the front of
;; the sequence.
;;
;; sequence-uncons : sequence -> any sequence
;; returns two values: the first element of the sequence, and a new
;; sequence containing all but the first element. If the sequence is
;; empty, a &sequence-empty condition is raised.
;;
;; sequence-snoc : sequence any -> sequence
;; return the new sequence created by adding the element to the end of
;; the sequence.
;;
;; sequence-unsnoc : sequence -> sequence any
;; returns two values: a new sequence containing all but the last
;; element of the sequence, and the last element itself. If the
;; sequence is empty, a &sequence-empty condition is raised.
;;
;; sequence-append : sequence sequence -> sequence
;; returns a new sequence containing all the elements of the first
;; sequence, followed by all the elements of the second sequence.
;;
;; list->sequence : Listof(Any) -> sequence
;; returns a new sequence containing all the elements of the argument
;; list, in the same order.
;;
;; sequence->list : sequence -> Listof(Any)
;; returns a new list containing all the elements of the sequence, in the
;; same order.
;;
;; sequence-split-at sequence integer -> sequence + sequence
;; returns two new sequences, the first containing the first N elements
;; of the sequence, the second containing the remaining elements. If N is
;; negative, it returns the empty sequence as the first argument, and the
;; original sequence as the second argument. Similarly, if N is greater
;; than the length of the list, it returns the original sequence as the
;; first argument, and the empty sequence as the second argument.
;;
;; Consequently, (let-values (((a b) (sequence-split-at s i)))
;; (sequence-append a b)) is equivalent to s for all sequences s, and
;; integers i.
;;
;; sequence-take sequence integer -> sequence
;; returns a new sequence containing the first N elements of the
;; argument sequence. If N is negative, the empty sequence is
;; returned. If N is larger than the length of the sequence, the whole
;; sequence is returned.
;;
;; sequence-drop sequence integer -> sequence
;; returns a new sequence containing all but the first N elements of the
;; argument sequence. If N is negative, the whole sequence is
;; returned. If N is larger than the length of the sequence, the empty
;; sequence is returned.
;;
;; sequence-ref : sequence non-negative-integer -> any
;; returns the element at the specified index in the sequence. If the
;; index is outside the range 0 <= i < (sequence-size sequence), an
;; assertion violation is raised.
;;
;; sequence-set : sequence non-negative-integer any -> sequence
;; returns the new sequence obtained by replacing the element at the
;; specified index in the sequence with the given value. If the index
;; is outside the range 0 <= i < (sequence-size sequence), an
;; assertion violation is raised.
;;
;; sequence-fold (any -> any -> any) any sequence
;; returns the value obtained by iterating the combiner procedure over
;; the sequence in left-to-right order. The combiner procedure takes two
;; arguments, the value of the position in the sequence, and an
;; accumulator, and its return value is used as the value of the
;; accumulator for the next call. The initial accumulator value is given
;; by the base argument.
;;
;; sequence-fold-right (any -> any -> any) any sequence
;; Like sequence-fold, but the sequence is traversed in right-to-left
;; order, rather than left-to-right.
;;
;; sequence-reverse : sequence -> sequence
;; returns a new sequence containing all the arguments of the argument
;; list, in reverse order.
;;
;; sequence-map : (any -> any) sequence -> sequence
;; returns a new sequence obtained by applying the procedure to each
;; element of the argument sequence in turn.
;;
;; sequence-filter : (any -> bool) sequence -> sequence
;; returns a new sequence containing all the elements of the argument
;; sequence for which the predicate is true.
;;
;; sequence-empty-condition? : any -> bool
;; returns #t if an object is a &sequence-empty condition, #f otherwise.
;;
(library (pfds sequences)
(export make-sequence
        sequence?
        sequence-empty?
        sequence-size
        sequence-cons
        sequence-uncons
        sequence-snoc
        sequence-unsnoc
        sequence-append
        list->sequence
        sequence->list
        (rename (%sequence sequence))
        sequence-split-at
        sequence-take
        sequence-drop
        sequence-ref
        sequence-set
        sequence-fold
        sequence-fold-right
        sequence-reverse
        sequence-map
        sequence-filter
        sequence-empty-condition?
        )

(import (rnrs)
        (pfds fingertrees))

;; Note: as sequences are not a subtype of fingertrees, but rather a
;; particular instantiation of them, &sequence-empty is not a subtype
;; of &fingertree-empty
(define-condition-type &sequence-empty
  &assertion
  make-sequence-empty-condition
  sequence-empty-condition?)

(define-record-type (sequence %make-sequence sequence?)
  (fields fingertree))

(define (make-sequence)
 (%make-sequence (make-fingertree 0 + (lambda (x) 1))))

(define (sequence-empty? seq)
  (fingertree-empty? (sequence-fingertree seq)))

(define (sequence-size seq)
  (fingertree-measure (sequence-fingertree seq)))

(define (sequence-cons value seq)
  (%make-sequence
   (fingertree-cons value (sequence-fingertree seq))))

(define (sequence-snoc seq value)
  (%make-sequence
   (fingertree-snoc (sequence-fingertree seq) value)))

(define (sequence-uncons seq)
  (call-with-values
      (lambda ()
        (define ft (sequence-fingertree seq))
        (when (fingertree-empty? ft)
          (raise
           (condition
            (make-sequence-empty-condition)
            (make-who-condition 'sequence-uncons)
            (make-message-condition "There are no elements to uncons")
            (make-irritants-condition (list seq)))))
        (fingertree-uncons ft))
    (lambda (head tree)
      (values head (%make-sequence tree)))))

(define (sequence-unsnoc seq)
  (call-with-values
      (lambda ()
        (define ft (sequence-fingertree seq))
        (when (fingertree-empty? ft)
          (raise
           (condition
            (make-sequence-empty-condition)
            (make-who-condition 'sequence-unsnoc)
            (make-message-condition "There are no elements to unsnoc")
            (make-irritants-condition (list seq)))))
        (fingertree-unsnoc ft))
    (lambda (tree last)
      (values (%make-sequence tree) last))))

(define (sequence-append seq1 seq2)
  (%make-sequence
   (fingertree-append (sequence-fingertree seq1)
                      (sequence-fingertree seq2))))

(define (list->sequence list)
  (fold-left sequence-snoc
             (make-sequence)
             list))

(define (sequence->list seq)
  (fingertree->list (sequence-fingertree seq)))

(define (%sequence . args)
  (list->sequence args))

(define (sequence-split-at seq i)
  (let-values (((l r)
                (fingertree-split (lambda (x) (< i x))
                                  (sequence-fingertree seq))))
    (values (%make-sequence l)
            (%make-sequence r))))

(define (sequence-take seq i)
  (let-values (((head tail)
                (sequence-split-at seq i)))
    head))

(define (sequence-drop seq i)
  (let-values (((head tail)
                (sequence-split-at seq i)))
    tail))

(define (sequence-ref seq i)
  (define size (sequence-size seq))
  (unless (and (<= 0 i) (< i size))
    (assertion-violation 'sequence-ref "Index out of range" i))
  (let-values (((_l x _r)
                (fingertree-split3 (lambda (x) (< i x))
                                   (sequence-fingertree seq))))
    x))

(define (sequence-set seq i val)
  (define size (sequence-size seq))
  (unless (and (<= 0 i) (< i size))
    (assertion-violation 'sequence-set "Index out of range" i))
  (let-values (((l x r)
                (fingertree-split3 (lambda (x) (< i x))
                                   (sequence-fingertree seq))))
    (%make-sequence
     (fingertree-append l (fingertree-cons val r)))))

(define (sequence-fold proc base seq)
  (fingertree-fold proc base (sequence-fingertree seq)))

(define (sequence-fold-right proc base seq)
  (fingertree-fold-right proc base (sequence-fingertree seq)))

(define (sequence-reverse seq)
  (%make-sequence (fingertree-reverse (sequence-fingertree seq))))

(define (sequence-map proc seq)
  (define (combine element seq)
    (sequence-cons (proc element) seq))
  (sequence-fold-right combine (make-sequence) seq))

(define (sequence-filter pred? seq)
  (define (combine element seq)
    (if (pred? element)
        (sequence-cons element seq)
        seq))
  (sequence-fold-right combine (make-sequence) seq))

)
