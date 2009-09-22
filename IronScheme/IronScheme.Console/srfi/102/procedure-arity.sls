(library (srfi :102 procedure-arity)
  (export procedure-arity
          arity-at-least?
          arity-at-least-value
          procedure-arity-includes?)
  (import (rename (ironscheme) (procedure-arity is:procedure-arity)))
  
  (define (procedure-arity proc)
    (let-values ((a (is:procedure-arity proc))) 
      (cond
        [(null? a) #f]
        [(null? (cdr a))
          (let ((arity (car a)))
            (if (unspecified? arity)
                #f
                arity))]
        [else a])))

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
    (let-values ((a (is:procedure-arity proc)))
      (exists 
        (lambda (x)
          (if (exact? x)
              (= k x)
              (< x k)))))))
