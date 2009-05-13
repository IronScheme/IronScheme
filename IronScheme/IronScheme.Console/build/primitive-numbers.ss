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
    (ironscheme clr))
    
  (define (flonum? obj)
    (clr-is system.double obj))     

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
            (let f ((a (combine (car lst) nil))(lst (cdr lst)))
              (if (null? lst)
                  a
                  (f (combine a (car lst)) (cdr lst)))))]))
      
      
  (define +
    (case-lambda
      [() 0]
      [(num)
        (unless (number? num)
          (assertion-violation '+ "not a number" num))
        num]
      [(num1 num2)                   
        (unless (number? num1)
          (assertion-violation '+ "not a number" num1))
        (unless (number? num2)
          (assertion-violation '+ "not a number" num2))
        (generic+ num1 num2)]
      [(num1 num2 num3 . rest)
        (reduce + (+ (+ num1 num2) num3) rest)]))
        
  (define -
    (case-lambda
      [(num)
        (unless (number? num)
          (assertion-violation '- "not a number" num))
        (generic- 0 num)]
      [(num1 num2)                   
        (unless (number? num1)
          (assertion-violation '- "not a number" num1))
        (unless (number? num2)
          (assertion-violation '- "not a number" num2))
        (generic- num1 num2)]
      [(num1 num2 num3 . rest)
        (reduce - (- (- num1 num2) num3) rest)]))
        
  (define *
    (case-lambda
      [() 1]
      [(num)
        (unless (number? num)
          (assertion-violation '* "not a number" num))
        num]
      [(num1 num2)                   
        (unless (number? num1)
          (assertion-violation '* "not a number" num1))
        (unless (number? num2)
          (assertion-violation '* "not a number" num2))
        (generic* num1 num2)]
      [(num1 num2 num3 . rest)
        (reduce * (* (* num1 num2) num3) rest)]))                

  (define /
    (case-lambda
      [(num)
        (unless (number? num)
          (assertion-violation '/ "not a number" num))
        (generic/ 1 num)]
      [(num1 num2)                   
        (unless (number? num1)
          (assertion-violation '/ "not a number" num1))
        (unless (number? num2)
          (assertion-violation '/ "not a number" num2))
        (generic/ num1 num2)]
      [(num1 num2 num3 . rest)
        (/ num1 (reduce * (* num2 num3) rest))])) 
                          
   
)
  
  