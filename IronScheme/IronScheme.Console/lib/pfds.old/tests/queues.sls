#!r6rs
(library (pfds tests queues)
(export queues)
(import (rnrs)
        (wak trc-testing)
        (pfds tests utils)
        (pfds queues))

(define-test-suite queues
  "Tests for the functional queue implementation")

(define-test-case queues empty-queue ()
  (test-predicate queue? (make-queue))
  (test-predicate queue-empty? (make-queue))
  (test-eqv 0 (queue-length (make-queue))))

(define-test-case queues enqueue
  (let ((queue (enqueue (make-queue) 'foo)))
    (test-case enqueue ()
      (test-predicate queue? queue)
      (test-eqv #t (not (queue-empty? queue)))
      (test-eqv 1 (queue-length queue))
      (test-eqv 10 (queue-length
                    (foldl (lambda (val queue)
                             (enqueue queue val))
                           (make-queue)
                           '(0 1 2 3 4 5 6 7 8 9)))))))

(define-test-case queues dequeue ()
  (let ((empty (make-queue))
        (queue1 (enqueue (make-queue) 'foo))
        (queue2 (enqueue (enqueue (make-queue) 'foo) 'bar)))
    (let-values (((item queue) (dequeue queue1)))
      (test-eqv 'foo item)
      (test-predicate queue? queue)
      (test-predicate queue-empty? queue))
    (let*-values (((first queue*) (dequeue queue2))
                  ((second queue) (dequeue queue*)))
                 (test-eqv 'foo first)
                 (test-eqv 'bar second)
                 (test-eqv 1 (queue-length queue*))
                 (test-eqv 0 (queue-length queue)))
    (test-eqv #t
              (guard (exn ((queue-empty-condition? exn) #t)
                          (else #f))
                (dequeue empty)
                #f))))


(define-test-case queues queue-ordering ()
  (let* ((list '(bar quux foo zot baz))
         (queue (list->queue list)))
    (test-eqv 5 (queue-length queue))
    (test-equal list (queue->list queue))))

)
