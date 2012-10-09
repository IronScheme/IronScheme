;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (srfi :43 vectors)
  (export
    ;;; * Constructors
    make-vector vector
    vector-unfold         vector-unfold-right
    vector-copy           vector-reverse-copy
    vector-append         vector-concatenate
    ;;; * Predicates
    vector?
    vector-empty?
    vector=
    ;;; * Selectors
    vector-ref
    vector-length
    ;;; * Iteration
    vector-fold           vector-fold-right
    vector-map            vector-map!
    vector-for-each
    vector-count
    ;;; * Searching
    vector-index          vector-skip
    vector-index-right    vector-skip-right
    vector-binary-search  vector-any    vector-every
    ;;; * Mutators
    vector-set!
    vector-swap!
    vector-fill!
    vector-reverse!
    vector-copy!          vector-reverse-copy!
    ;;; * Conversion
    vector->list          reverse-vector->list
    list->vector          reverse-list->vector )
  (import
    (except (rnrs) vector-map vector-for-each vector-fill! vector->list
                   list->vector)
    (prefix (only (rnrs) vector-fill! vector->list list->vector) rnrs:)
    (rnrs r5rs)
    (srfi :23 error tricks)
    (srfi :8 receive)
    (srfi private include))
  
  (define-syntax check-type
    (lambda (stx)
      (syntax-case stx ()
        [(_ pred? value callee)
         (if (identifier? #'value)
           #'(if (pred? value)
               value
               (assertion-violation callee "erroneous value" value))
           #'(let ([v value])
               (if (pred? v)
                 v
                 (assertion-violation callee "erroneous value" v))))])))
    
  (SRFI-23-error->R6RS "(library (srfi :43 vectors))"
   (include/resolve ("srfi" "43") "vector-lib.scm"))
)
