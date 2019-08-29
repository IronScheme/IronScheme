#!r6rs
;;; vectors.sls --- Vector Utilities

;; Copyright (C) 2014 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.
(library (pfds private vectors)
(export vector-set
        vector-insert
        vector-remove
        vector-copy
        vector-copy!
        vector-fold
        )
(import (rnrs base)
        (rnrs control))

(define (vector-set h i x)
  (let ((v* (vector-copy h)))
    (vector-set! v* i x)
    v*))

(define (vector-remove v i)
  (define len (vector-length v))
  (assert (and (<= 0 i) (< i len)))
  (let ((newvec (make-vector (- len 1))))
    (vector-copy! v 0 newvec 0 i)
    (vector-copy! v (+ i 1) newvec i (- len i 1))
    newvec))

(define (vector-insert v i x)
  (define len (vector-length v))
  (assert (<= 0 i len))
  (let* ((newvec (make-vector (+ len 1))))
    (vector-set! newvec i x)
    (vector-copy! v 0 newvec 0 i)
    (vector-copy! v i newvec (+ 1 i) (- len i))
    newvec))

(define (vector-copy! source source-start target target-start k)
  ;; TODO: assertions
  ;; guile has vector-move functions, but rnrs does not :(
  (do ((i 0 (+ 1 i)))
      ((>= i k))
    (vector-set! target
                 (+ target-start i)
                 (vector-ref source (+ source-start i)))))

(define (vector-copy vector)
  (define len (vector-length vector))
  (define v* (make-vector len))
  (vector-copy! vector 0 v* 0 len)
  v*)

;; vector-fold is left to right
(define (vector-fold combine initial vector)
  (define len (vector-length vector))
  (let loop ((index 0) (accum initial))
    (if (>= index len)
        accum
        (loop (+ index 1)
              (combine (vector-ref vector index) accum)))))

)
