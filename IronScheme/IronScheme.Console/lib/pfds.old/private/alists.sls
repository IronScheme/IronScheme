#!r6rs
;;; alists.sls --- Alist Utilities

;; Copyright (C) 2014 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.
(library (pfds private alists)
(export alist-ref
        alist-set
        alist-delete
        alist-update
        )
(import (rnrs base)
        (only (srfi :1 lists) assoc)
        )

(define (alist-ref alist key default eqv?)
  (cond ((assoc key alist eqv?) => cdr)
        (else default)))

(define (alist-set alist key value eqv?)
  ;; TODO: measure to see if it is even worth it
  ;; adds key value to alist if key is not in alist
  ;; if key is in a list, replaces the association
  ;; does not preserve order.
  (let loop ((new '()) (old alist))
    (cond ((null? old)
           (cons (cons key value) new))
          ((eqv? (car (car old)) key)
           (cons (cons key value)
                 (append (cdr old) new)))
          (else (loop (cons (car old) new) (cdr old))))))

;;((al (alist-delete (collision-alist node) key eqv?)))
(define (alist-delete alist key eqv?)
  ;; TODO: measure to see if it is even worth it
  (let loop ((new '()) (old alist))
    (cond ((null? old) new)
          ((eqv? (car (car old)) key)
           (append (cdr old) new))
          (else (loop (cons (car old) new) (cdr old))))))

(define (alist-update alist key update base eqv?)
  (let loop ((new '()) (old alist))
    (cond ((null? old)
           (cons (cons key (update base)) new))
          ((eqv? (car (car old)) key)
           (cons (cons key (update (cdr (car old))))
                 (append (cdr old) new)))
          (else (loop (cons (car old) new) (cdr old))))))


)
