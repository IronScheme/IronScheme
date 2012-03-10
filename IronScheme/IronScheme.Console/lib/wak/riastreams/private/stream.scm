;;; -*- Mode: Scheme; scheme48-package: lazy-streams -*-

;;;; Lazy Streams

;;; This code is written by Taylor R. Campbell and Andreas Rottmann
;;; and placed in the Public Domain.  All warranties are disclaimed.
;;;

(define-syntax stream-cons
  (syntax-rules ()
    ((STREAM-CONS a d)
     (DELAY (CONS a d)))))

(define stream-nil (delay '()))

(define-syntax define-stream-unop
  (syntax-rules ()
    ((DEFINE-STREAM-UNOP stream-op op)
     (DEFINE (stream-op STREAM) (op (FORCE STREAM))))))

(define-stream-unop stream-null?        null?)
(define-stream-unop stream-pair?        pair?)
(define-stream-unop stream-car          car)
(define-stream-unop stream-cdr          cdr)

(define (stream->list stream)
  (let ((datum (force stream)))
    (if (pair? datum)
        (cons (car datum)
              (stream->list (cdr datum)))
        datum)))

(define (list->stream list)
  (lazy (if (pair? list)
            (stream-cons (car list)
                         (list->stream (cdr list)))
            (eager list))))

(define (string->stream string)
  (let recur ((index 0))
    (lazy (if (= index (string-length string))
              stream-nil
              (stream-cons (string-ref string index)
                           (recur (+ index 1)))))))

(define (vector->stream vector)
  (let recur ((index 0))
    (lazy (if (= index (vector-length vector))
              stream-nil
              (stream-cons (vector-ref vector index)
                           (recur (+ index 1)))))))

;** Be careful!  This operation is potentially dangerous.

(define (stream-difference earlier later)
  (lazy (if (eq? earlier later)
            stream-nil
            (stream-cons (stream-car earlier)
                         (stream-difference (stream-cdr earlier)
                                            later)))))
(define (stream-append . streams)
  (let outer-recur ((streams streams))
    (if (pair? streams)
        (let ((stream (car streams))
              (streams (cdr streams)))
          (let inner-recur ((stream stream))
            (lazy (if (stream-pair? stream)
                      (stream-cons (stream-car stream)
                                   (lazy (inner-recur (stream-cdr stream))))
                      (outer-recur streams)))))
        stream-nil)))

;; foof-loop iterator
(define-syntax in-stream
  (syntax-rules ()
    ((_ (elt-var stream-var) (stream-expr) cont . env)
     (cont
      ()                                    ;Outer bindings
      ((stream-var stream-expr              ;Loop variables
                   (stream-cdr stream-var)))
      ()                                    ;Entry bindings
      ((stream-null? stream-var))           ;Termination conditions
      (((elt-var) (stream-car stream-var))) ;Body bindings
      ()                                    ;Final bindings
      . env))
    ;; Optional stream variable is optional
    ((_ (elt-var) (stream-expr) cont . env)
     (in-stream (elt-var stream) (stream-expr) cont . env))))
