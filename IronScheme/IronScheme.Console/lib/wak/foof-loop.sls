;;; foof-loop.sls --- 

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (wak foof-loop)
  (export
    loop
    lazy-loop

    =>
    for
    with
    until
    let
    let-values
    while

    listing
    listing-reverse
    appending
    appending-reverse
    listing!
    listing-into!
    summing
    multiplying
    maximizing
    minimizing

    initial

    up-from
    down-from

    to
    by

    in-list
    in-lists
    in-vector
    in-vector-reverse
    in-string
    in-string-reverse
    in-port
    in-file
    )
  (import
    (rnrs)
    (only (rnrs mutable-pairs) set-cdr!)
    (srfi :8 receive)
    (srfi :45 lazy)
    (wak syn-param)
    (wak private include))

  (define-syntax define-aux
    (syntax-rules ()
      ((_ id ...)
	(begin
	  (define-syntax id
	    (lambda (x)
	      (syntax-violation #f "invalid use of auxiliary keyword" x 'id)))
	  ...))))

  (define-aux
    for
    with
    to
    by
    until
    while
    initial
    )

  (include-file/downcase ((wak foof-loop private) foof-loop))

)
