#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme primitive-numbers)
  (export
    +
    -
    *
    /)
  (import 
    (except 
      (rnrs)
      +
      -
      *
      /)
    (ironscheme core)
    (ironscheme unsafe)
    (ironscheme contracts)
    (ironscheme clr))

  (define-syntax reduce
    (syntax-rules ()
      [(_ combine nil lst)
        (if (null? lst)
            nil
            (let f ((a (combine nil (car lst)))(lst (cdr lst)))
              (if (null? lst)
                  a
                  (f (combine a (car lst)) (cdr lst)))))]))
  
  (define/contract +
    (case-lambda
      [() 0]
      [(num:number)
        num]
      [(num1:number num2:number)
        (generic+ num1 num2)]
      [(num1 num2 num3 . rest)
        (reduce + (+ (+ num1 num2) num3) rest)]))
        
  (define/contract -
    (case-lambda
      [(num:number)
        (generic- 0 num)]
      [(num1:number num2:number)
        (generic- num1 num2)]
      [(num1 num2 num3 . rest)
        (reduce - (- (- num1 num2) num3) rest)]))
        
  (define/contract *
    (case-lambda
      [() 1]
      [(num:number)
        num]
      [(num1:number num2:number)                   
        (generic* num1 num2)]
      [(num1 num2 num3 . rest)
        (reduce * (* (* num1 num2) num3) rest)]))                
        
  (define (exact-zero? num)
    (and (exact? num) (zero? num)))        

  (define/contract /
    (case-lambda
      [(num:number)
        (generic/ 1 num)]
      [(num1:number num2:number)
        (when (and (exact? num1) (exact-zero? num2))
          (assertion-violation '/ "divide by zero" num1 num2))
        (if (and (zero? num1) (zero? num2))
            +nan.0
            (generic/ num1 num2))]
      [(num1 num2 num3 . rest)
        (/ num1 (reduce * (* num2 num3) rest))])))
  
   