#!r6rs

(library (tests r6rs contrib)
  (export run-contrib-tests)
  (import (rnrs)
          (tests r6rs test)
          (prefix (tests r6rs contrib helper1) L:))

  ;; Definitions ----------------------------------------

  ;; from Derick Eddington:
  (define-syntax my-letrec
    (syntax-rules ()
      [(_ ([v e] ...) . b)
       (let ()
         (define t (list e ...))
         (define v (let ([v (car t)]) (set! t (cdr t)) v))
         ...
         . b)]))

  ;; Expressions ----------------------------------------

  (define (run-contrib-tests)

    ;; from Derick Eddington:
    (test (my-letrec ([f (lambda (x) (g x 2))]
                      [g (lambda (x y) (+ x y))])
            (f 1))
          3)

    ;; from Derick Eddington:
    (test (L:s L:x) 'ok)


    ;; from weinholt
    (test/values (div-and-mod -3 9) -1 6)
    (test/values (div0-and-mod0 -3 9) 0 -3)
    
    (test/values (fxdiv-and-mod -3 9) -1 6)
    (test/values (fxdiv0-and-mod0 -3 9) 0 -3)
    
    (test (max +inf.0 +nan.0) +nan.0)
    (test (min +inf.0 +nan.0) +nan.0)
    
    (test (flmax +inf.0 +nan.0) +nan.0)
    (test (flmin +inf.0 +nan.0) +nan.0)
    
    (test (round 1/2) 0)
    (test (round -1/2) 0)
    
    
    (test (bitwise-bit-count 0) 0)
    (test (fxbit-count 0) 0)
    
    (test (bitwise-rotate-bit-field 1 1 1 1) 1)
    
    (test (fxbit-set? -1 100) #t)
    
    (test/exn (fxarithmetic-shift-right 42 (fixnum-width)) &assertion)
    (test/exn (fxarithmetic-shift-left 42 (fixnum-width)) &assertion)
    
    (test (expt -2 -inf.0) 0.0)
    
    (test/exn (integer->char #x10000) &implementation-restriction)
    
    (test (fl<=? +inf.0 +nan.0) (fl<? +inf.0 +nan.0))
    
    (test/values (fxdiv-and-mod -1720408098 -1532586397) 2 1344764696)
    
    (test (fxdiv -1720408098 -1532586397) 2)
    (test (fxmod -1720408098 -1532586397) 1344764696)
    
    (test/values (exact-integer-sqrt 4294967312) 65536 16)
    ;;;
    ))
