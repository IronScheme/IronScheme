#!r6rs
(library (pfds tests utils)
(export pfds
        test
        test-not
        test-exn
        test-no-exn
        add1
        foldl
        iota
        )
(import (rnrs)
        (wak trc-testing))

(define-test-suite pfds
  "Test suite for libraries under the (pfds) namespace")

(define-syntax test
  (syntax-rules ()
    ((test body)
     (test-eqv #t (and body #t)))))

(define-syntax test-not
  (syntax-rules ()
    ((test-not body)
     (test-eqv #f body))))

(define-syntax test-exn
  (syntax-rules ()
    ((test-exn exception-pred? body)
     (test-eqv #t
               (guard (exn ((exception-pred? exn) #t)
                           (else #f))
                 body
                 #f)))))

(define-syntax test-no-exn
  (syntax-rules ()
    ((test-no-exn body)
     (test-eqv #t
               (guard (exn (else #f))
                 body
                 #t)))))

(define (add1 x)
  (+ x 1))

(define (foldl kons knil list)
  (if (null? list)
      knil
      (foldl kons (kons (car list) knil) (cdr list))))

(define (iota n)
  (define (recur x)
    (if (< x n)
        (cons x (recur (+ x 1)))
        '()))
  (assert (integer? n))
  (recur 0))

)
