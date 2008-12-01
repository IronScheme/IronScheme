(import 
  (ironscheme)
  (ironscheme clr)
  (ironscheme strings)
  (ironscheme contracts)
  (ironscheme clr reflection))

(clr-using System)  
(clr-using System.Reflection.Emit)  

(define/contract (make-dynamic-method name return-type:clr-type . param-types:clr-type)
  (clr-new DynamicMethod (symbol->string name) return-type (list->vector param-types)))
    

(define dm (make-dynamic-method 'int-proc (get-clr-type 'System.Object) (get-clr-type 'System.Object)))

(define (get-delegate-type dm)
  (let ((p (method-params dm)))
    (case (length p)
      [(0) (get-clr-type 'Microsoft.Scripting.CallTarget0)]
      [(1) (get-clr-type 'Microsoft.Scripting.CallTarget1)]
      [(2) (get-clr-type 'Microsoft.Scripting.CallTarget2)]
      [(3) (get-clr-type 'Microsoft.Scripting.CallTarget3)]
      [(4) (get-clr-type 'Microsoft.Scripting.CallTarget4)]
      [(5) (get-clr-type 'Microsoft.Scripting.CallTarget5)])))

(define (make-proc dm)
  (let ((delegate (clr-call DynamicMethod CreateDelegate dm (get-delegate-type dm))))
    (clr-static-call IronScheme.Runtime.Closure MakeStatic delegate)))
    
(define (opcode? obj)
  (clr-is OpCode obj))
  
(define (opcode-operand-type opcode)
  (clr-prop-get OpCode OperandType opcode))
  
(define (opcode-type opcode)
  (clr-prop-get OpCode OpCodeType opcode))
  
(define (make-field-ref name) #f)  
(define (make-method-ref name) #f) 

(define (get-operand-type opcode)
  (case (opcode-operand-type opcode)
    [(inlinenone) #f]
    [(inlinefield)  make-field-ref]
    [(inlinemethod) make-method-ref]
    [(inlinestring) values]
    [(inliner)      values]
    [else 
      (assertion-violation 'get-operand-type "todo" opcode)]))

(define (il-gen? obj)
  (clr-is ILGenerator obj))  
  
(define (get-il-gen dm)
  (clr-call DynamicMethod GetILGenerator dm))    


(define-syntax opcode
  (lambda (x)
    (define (opcode-name name)
      (datum->syntax name 
        (string->symbol 
          (string-replace 
            (symbol->string (syntax->datum name)) 
            "." 
            "_"))))
    (syntax-case x ()
      [(_ name)
        (identifier? #'name)
        (with-syntax ((name (opcode-name #'name)))
          #'(clr-static-field-get OpCodes name))])))
        
        
(define emit
  (case/contract 
    [(gen:il-gen op:opcode)
      (clr-call ILGenerator "Emit(OpCode)" il-gen op)]))
      
(define il-gen (get-il-gen dm))

(emit il-gen (opcode ldarg.0))
(emit il-gen (opcode ret))

(define my-proc (make-proc dm))
