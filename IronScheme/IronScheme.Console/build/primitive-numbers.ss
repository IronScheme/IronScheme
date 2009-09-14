#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

(library (ironscheme primitive-numbers)
  (export
    +
    -
    *
    /
    number?)
  (import 
    (except 
      (rnrs)
      +
      -
      *
      /
      flonum?
      number?)
    (except 
      (ironscheme core))
    (ironscheme unsafe)
    (ironscheme contracts)
    (ironscheme clr))
    
  (define (flonum? obj)
    (clr-is Double obj))     

  (define (bignum? obj)
    (clr-is Microsoft.Scripting.Math.BigInteger obj))
    
  (define (rectnum? obj)
    (clr-is IronScheme.Runtime.ComplexFraction obj)) 
  
  (define (ratnum? obj)
    (clr-is IronScheme.Runtime.Fraction obj))
  
  (define (complexnum? obj)
    (clr-is Microsoft.Scripting.Math.Complex64 obj))
    
  (define (number? obj)
    (or (fixnum? obj)
        (flonum? obj)
        (bignum? obj)
        (ratnum? obj)
        (complexnum? obj)
        (rectnum? obj)))    

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
        (/ num1 (reduce * (* num2 num3) rest))])) 
                          
   
)
  
   