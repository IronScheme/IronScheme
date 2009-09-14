(import (ironscheme)
        (ironscheme clr)
        (ironscheme clr reflection))
        
(clr-reference "System.Core, Version=3.5.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")

(clr-using System.Reflection)
(clr-using System.Linq.Expressions)

(define (expr? obj)
  (clr-is Expression obj))
  
(define (binary-expr? obj)
  (clr-is BinaryExpression obj))
  
  
(define Callable-Type (get-clr-type 'IronScheme.Runtime.Callable))

(define call-map
  (let* ((mems (type-member Callable-Type "Call" 'method))
         (vecl (length mems))
         (vec  (make-vector vecl)))
    (for-each (lambda (mem)
                (let* ((pars (method-params mem))
                       (plen (length pars)))
                  (case plen
                    [(1)
                      (if (type-array? (param-type (car pars)))
                          (vector-set! vec (- vecl 1) mem)
                          (vector-set! vec plen mem))]
                    [else
                      (vector-set! vec plen mem)])))
              mems)
    vec))

(define (get-call-meth argcount)
  (vector-ref call-map argcount))

(define (Add expr1 expr2)
  (clr-static-call Expression (Add Expression Expression) expr1 expr2))
  
(define (And expr1 expr2)
  (clr-static-call Expression (And Expression Expression) expr1 expr2))  
  
(define (AndAlso expr1 expr2)
  (clr-static-call Expression (AndAlso Expression Expression) expr1 expr2))  
  
(define (ArrayIndex array index)
  (clr-static-call Expression (ArrayIndex Expression Expression) array index))  

(define (Bind proc expr)
  (let ((meth (get-call-meth 2)))
    (clr-static-call Expression (Bind MethodInfo Expression) meth expr)))  
  
(define (Call proc . args)
  (let ((meth (get-call-meth (length args))))
    (clr-static-call Expression 
                     (Call Expression MethodInfo Expression[]) 
                     (Constant proc) 
                     meth 
                     (list->vector args))))

(define (Constant obj)
  (clr-static-call Expression (Constant Object) obj))                     
    
    
  