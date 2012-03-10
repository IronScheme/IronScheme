#!r6rs
;;; dlists.sls --- Difference Lists

;; Copyright (C) 2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:
;;
;; Repeatedly appending to a list is a common, if inefficient pattern
;; in functional programs. Usually the trick we use is to build up the
;; list in reverse, and then to reverse it as the last action of a
;; function.
;;
;; Dlists are a representation of lists as functions that provide for
;; constant time append to either the front or end of a dlist that may
;; be used instead.

;;; Documentation:
;;
;; dlist : any ... -> dlist
;; returns a dlist containing all its arguments.
;;
;; dlist? : any -> boolean
;; returns #t if its argument is a dlist, #f otherwise.
;;
;; dlist-cons : any dlist -> dlist
;; returns a new dlist created by prepending the element to the head
;; of the dlist argument.
;;
;; dlist-snoc : dlist any -> dlist
;; returns a new dlist created by appending the element to the tail of
;; the dlist argument.
;;
;; dlist-append : dlist dlist -> dlist
;; returns a new dlist consisting of all the elements of the first
;; dlist, followed by all the items of the second dlist.
;;
;; dlist->list : dlist -> listof(any)
;; returns a list consisting of all the elements of the dlist.
;;
;; list->dlist : listof(any) -> dlist
;; returns a dlist consisting of all the elements of the list.
(library (pfds dlists)
(export (rename (%dlist dlist))
        dlist?
        dlist-cons
        dlist-snoc
        dlist-append
        dlist->list
        list->dlist
        )
(import (rnrs))

(define-record-type dlist
  (fields
   (immutable proc undl)))

(define (%dlist . args)
  (list->dlist args))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (singleton x)
  (list->dlist (list x)))

(define (dlist-append dl1 dl2)
  (make-dlist (compose (undl dl1) (undl dl2))))

(define (dlist-cons element dlist)
  (dlist-append (singleton element) dlist))

(define (dlist-snoc dlist element)
  (dlist-append dlist (singleton element)))

(define (dlist->list dlist)
  ((undl dlist) '()))

(define (list->dlist list)
  (make-dlist
   (lambda (rest)
     (append list rest))))

)
