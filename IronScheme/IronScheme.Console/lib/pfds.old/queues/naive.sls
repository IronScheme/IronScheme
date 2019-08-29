#!r6rs
;;; queues.sls --- Purely functional queues

;; Copyright (C) 2013 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; This code shares the same interface as (pfds queues). Please see
;; that module for documentation of the interface. The difference is
;; in performance characteristics.

;; The version in (pfds queues) uses memoisation to ensure that a
;; dequeue operation always takes less than a constant bound,
;; regardless of access pattern. This module *may* be more expensive
;; in certain access patterns, due to use of reverse, *but* in the
;; common stateful pattern of only ever using the most recent version
;; of the queue, it can be cheaper.
;;
;; If in doubt, benchmark.

;;; Code:
(library (pfds queues naive)
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
(import (rnrs)
        (pfds queues private condition))

(define-record-type (queue %make-queue queue?)
  (fields length head tail))

(define (make-queue)
  (%make-queue 0 '() '()))

(define (queue-empty? queue)
  (zero? (queue-length queue)))

(define (enqueue queue object)
  (%make-queue (+ 1 (queue-length queue))
               (queue-head queue)
               (cons object (queue-tail queue))))

(define (dequeue queue)
  (when (queue-empty? queue)
    (raise (condition
            (make-queue-empty-condition)
            (make-who-condition 'dequeue)
            (make-message-condition "There are no elements to dequeue")
            (make-irritants-condition (list queue)))))
  (let ((l (queue-length queue))
        (h (queue-head queue))
        (t (queue-tail queue)))
    (if (null? h)
        (let ((h* (reverse t)))
          (values (car h*)
                  (%make-queue (- l 1) (cdr h*) '())))
        (values (car h)
                (%make-queue (- l 1) (cdr h) t)))))

(define (list->queue l)
  (%make-queue (length l) l '()))

(define (queue->list queue)
  (let ((h (queue-head queue))
        (t (queue-tail queue)))
    (append h (reverse t))))

)
