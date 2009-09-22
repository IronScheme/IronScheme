(library (srfi :102 procedure-arity)
  (export procedure-arity
          arity-at-least?
          arity-at-least-value
          procedure-arity-includes?)
  (import (ironscheme))

  (define (arity-at-least? obj)
    (and (integer? obj)
         (inexact? obj)	 
         (not (negative? obj))))

  (define (arity-at-least-value arity-at-least)
    (assert (arity-at-least? arity-at-least))
    (inexact->exact arity-at-least))

  (define (procedure-arity-includes? proc k)
    (assert (procedure? proc))
    (assert (and (integer? k)
                 (exact? k)
                 (not (negative? k))))
    (let ((a (procedure-arity proc)))
      (and a
           (if (exact? a)
               (= k a)
               (< k a))))))
