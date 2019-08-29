#!r6rs
;;; queues.sls --- Purely functional queues

;; Copyright (C) 2011,2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:
;;
;; A scheme translation of "Simple and Efficient Purely Functional
;; Queues and Deques" by Chris Okazaki
;;
;;
;;; Documentation:
;;
;; make-queue : () -> queue
;; returns a queue containing no items
;;
;; queue? : any -> boolean
;; tests if an object is a queue
;;
;; queue-length : queue -> non-negative integer
;; returns the number of items in the queue
;;
;; queue-empty? : queue -> boolean
;; returns true if there are no items in the queue, false otherwise
;;
;; enqueue : queue any -> queue
;; returns a new queue with the enqueued item at the end
;;
;; dequeue : queue -> value queue
;; returns two values, the item at the front of the queue, and a new
;; queue containing the all the other items
;; raises a &queue-empty condition if the queue is empty
;;
;; queue-empty-condition? : object -> boolean
;; tests if an object is a &queue-empty condition
;;
;; queue->list : queue -> listof(any)
;; returns a queue containing all the items in the list. The order of
;; the elements in the queue is the same as the order of the elements
;; in the list.
;;
;; list->queue : listof(any) -> queue
;; returns a list containing all the items in the queue. The order of
;; the items in the list is the same as the order in the queue.
;; For any list l, (equal? (queue->list (list->queue l)) l) is #t.
;;
(library (pfds queues)
(export make-queue
        queue?
        queue-length
        queue-empty?
        enqueue
        dequeue
        queue-empty-condition?
        list->queue
        queue->list
        )
(import (except (rnrs) cons*)
        (pfds private lazy-lists)
        (pfds queues private condition)
        (rnrs r5rs))

(define (rotate l r a)
  (if (empty? l)
      (cons* (head r) a)
      (cons* (head l)
             (rotate (tail l)
                     (tail r)
                     (cons* (head r) a)))))


;;; Implementation

(define-record-type (queue %make-queue queue?)
  (fields
   (immutable length)
   (immutable l)
   (immutable r)
   (immutable l^)))


(define (make-queue)
  (%make-queue 0 '() '() '()))

(define (enqueue queue item)
  (let ((len (queue-length queue))
        (l (queue-l queue))
        (r (queue-r queue))
        (l^ (queue-l^ queue)))
    (makeq (+ len 1) l (cons* item r) l^)))

(define (dequeue queue)
  (when (queue-empty? queue)
    ;; (error 'dequeue  "Can't dequeue empty queue")
    (raise (condition
            (make-queue-empty-condition)
            (make-who-condition 'dequeue)
            (make-message-condition "There are no elements to dequeue")
            (make-irritants-condition (list queue)))))
  (let ((len (queue-length queue))
        (l (queue-l queue))
        (r (queue-r queue))
        (l^ (queue-l^ queue)))
    (values (head l)
            (makeq (- len 1) (tail l) r l^))))

(define (makeq length l r l^)
  (if (empty? l^)
      (let ((l* (rotate l r '())))
        (%make-queue length l* '() l*))
      (%make-queue length l r (tail l^))))

(define (queue-empty? queue)
  (zero? (queue-length queue)))

(define (list->queue list)
  (fold-left enqueue (make-queue) list))

(define (queue->list queue)
  (let loop ((rev-list '()) (queue queue))
    (if (queue-empty? queue)
        (reverse rev-list)
        (let-values (((val queue) (dequeue queue)))
          (loop (cons val rev-list)
                 queue)))))

)
