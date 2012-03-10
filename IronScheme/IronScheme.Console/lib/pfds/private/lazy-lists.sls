#!r6rs
;;; lazy-lists.sls --- odd lazy lists

;; Copyright (C) 2011 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; If you want real lazy lists, use SRFI 41, but Okazaki uses 'odd'
;; lists, so I wrote a quick implementation.

;;; Code:

(library (pfds private lazy-lists)
(export cons*
        tail
        head
        empty?
        take
        drop
        rev
        append*
        )
(import (except (rnrs) cons*)
        (rnrs r5rs)
        )

(define-syntax cons*
  (syntax-rules ()
    ((cons* a b)
     (cons a (delay b)))))

(define head car)

(define empty? null?)

(define (tail pair)
  (if (empty? pair)
      pair
      (force (cdr pair))))

(define (take n l)
  (if (zero? n)
      '()
      (cons* (head l)
             (take (- n 1) (tail l)))))

(define (drop n l)
  (if (zero? n)
      l
      (drop (- n 1) (tail l))))

(define (append* x y)
  (if (empty? x)
      y
      (cons* (head x)
             (append* (tail x) y))))

(define (rev l)
  (let loop ((l l) (a '()))
    (if (empty? l)
        a
        (loop (tail l) (cons* (head l) a)))))

)
