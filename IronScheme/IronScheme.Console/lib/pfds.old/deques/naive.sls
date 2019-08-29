#!r6rs
;;; naive.sls --- Purely functional deques.

;; Copyright (C) 2013 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; This code shares the same interface as (pfds deques). Please see
;; that module for documentation of the interface.

;; Like (pfds queues naive), this provides a naive version that may be
;; more expensive in certain access patterns, but can be cheaper in
;; some common ones.

;; As always, if in doubt, benchmark.

;;; Code:
(library (pfds deques naive)
(export make-deque
        deque?
        deque-length
        deque-empty?
        enqueue-front
        enqueue-rear
        dequeue-front
        dequeue-rear
        deque-empty-condition?
        deque->list
        list->deque
        )
(import (rnrs)
        (pfds deques private condition))

(define-record-type (deque %make-deque deque?)
  (fields length head tail))

(define (make-deque)
  (%make-deque 0 '() '()))

(define (deque-empty? deque)
  (zero? (deque-length deque)))

(define (enqueue-front deque object)
  (%make-deque (+ 1 (deque-length deque))
               (cons object (deque-head deque))
               (deque-tail deque)))

(define (enqueue-rear deque object)
  (%make-deque (+ 1 (deque-length deque))
               (deque-head deque)
               (cons object (deque-tail deque))))

(define (dequeue-front deque)
  (when (deque-empty? deque)
    (raise (condition
            (make-deque-empty-condition)
            (make-who-condition 'dequeue-front)
            (make-message-condition "There are no elements to dequeue")
            (make-irritants-condition (list deque)))))
  (let ((l (deque-length deque))
        (h (deque-head deque))
        (t (deque-tail deque)))
    (if (null? h)
        (let ((h* (reverse t)))
          (values (car h*)
                  (%make-deque (- l 1) (cdr h*) '())))
        (values (car h)
                (%make-deque (- l 1) (cdr h) t)))))

(define (dequeue-rear deque)
  (when (deque-empty? deque)
    (raise (condition
            (make-deque-empty-condition)
            (make-who-condition 'dequeue-rear)
            (make-message-condition "There are no elements to dequeue")
            (make-irritants-condition (list deque)))))
  (let ((l (deque-length deque))
        (h (deque-head deque))
        (t (deque-tail deque)))
    (if (null? t)
        (let ((t* (reverse h)))
          (values (car t*)
                  (%make-deque (- l 1) '() (cdr t*))))
        (values (car t)
                (%make-deque (- l 1) h (cdr t))))))

(define (list->deque l)
  (%make-deque (length l) l '()))

(define (deque->list deque)
  (let ((h (deque-head deque))
        (t (deque-tail deque)))
    (append h (reverse t))))

)
