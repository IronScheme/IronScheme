(import (ironscheme typed))

(define (test-lambda)
  (let ([f (lambda: 
            ((x : fixnum) (y : fixnum) rest ... -> fixnum)
             (writeln (length rest))
             (+ x y))])
    (displayln (procedure-form f))
    (f 10 20 2 1)
    (f 10 20)
    (f 10 20 2)
    f))
((test-lambda) 10 20 2 3 4 5)

(define test (lambda: ((x1 : flonum) rest ... -> flonum) (length rest)))


(define (test-lambda)
  (let ([f (case-lambda: 
            ;[() #f]
            [((x : fixnum) (y : fixnum) rest ... -> fixnum)
             (writeln (length rest))
             (+ x y)])])
    (displayln (procedure-form f))             
    (f 10 20 2 3 4 5)))
(test-lambda)

