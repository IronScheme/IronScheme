;;; nested-foof-loop.sls --- 

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (wak foof-loop nested)
  (export iterate*
          iterate
          iterate!
          iterate-values

          parallel
          nested

          recur*
          recur
          lazy-recur*
          lazy-recur
          recur-values

          collect-list
          collect-list-reverse
          collect-list!
          collect-list-into!
          collect-stream
          collect-vector
          collect-vector-of-length
          collect-string
          collect-string-of-length
          collect-display
          collect-sum
          collect-product
          collect-count
          collect-average
          collect-minimum
          collect-maximum)
  (import (rnrs)
          (only (rnrs mutable-pairs) set-cdr!)
          (only (srfi :1) append-reverse)
          (srfi :8 receive)
          (wak riastreams)
          (wak foof-loop)
          (wak private include))
  
  (define-syntax define-aux
    (syntax-rules ()
      ((_ id ...)
        (begin
          (define-syntax id
            (lambda (x)
              (syntax-violation #f "invalid use of auxiliary keyword" x 'id)))
          ...))))

  (define-aux parallel nested)

  (include-file/downcase ((wak foof-loop private) nested-foof-loop)))
