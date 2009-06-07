(import 
  (clr)
  (ironscheme)
  (ironscheme linq)
  (ironscheme clr)
  (ironscheme strings)
  (ironscheme contracts)
  (ironscheme clr reflection))

(clr-using System)  
(clr-using System.Reflection)
(clr-using System.Reflection.Emit)  

(define/contract (make-dynamic-method name return-type:clr-type . param-types:clr-type)
  (clr-new DynamicMethod (symbol->string name) return-type (list->vector param-types)))
    
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

(define emit (import-clr-method System.Reflection.Emit.ILGenerator Emit))        

(define (declare-local ilgen type)
  (clr-call System.Reflection.Emit.ILGenerator DeclareLocal ilgen type))
  
(define (define-label ilgen)
  (clr-call System.Reflection.Emit.ILGenerator DefineLabel ilgen))
  
(define (mark-label ilgen label)
  (clr-call System.Reflection.Emit.ILGenerator MarkLabel ilgen label))
  
(define (get-method type name . argtypes)
  (single
    (from c in (type-member type (symbol->string name))
     where (method? c) 
     let p = (method-params c)
     where (= (length argtypes) (length p))
     where (for-all eq? (map param-type p) argtypes)
     select c)))
  
     
(define (get-ctor type . argtypes)
  (single
    (from c in (type-member type ".ctor")
     where (constructor? c) 
     let p = (method-params c)
     where (= (length argtypes) (length p))
     where (for-all eq? (map param-type p) argtypes)
     select c)))
     
(define-syntax type
  (lambda (x)
    (syntax-case x ()
      [(_ class)
        #'(get-clr-type 'class)])))     
     
(define-syntax ctor
  (lambda (x)
    (syntax-case x ()     
      [(_ class argtype ...)
        #'(get-ctor (type class) (type argtype) ...)])))
        
(define-syntax method
  (lambda (x)
    (syntax-case x ()
      [(_ class name argtype ...)
        #'(get-method (type class) 'name (type argtype) ...)])))
     
(define-syntax il-method
  (lambda (x)
    (define (get-labels opcodes)
      (let f ((x opcodes)(a '()))
        (if (null? x)
            (reverse a)
            (syntax-case (car x) (label)
              [(label name)
                (f (cdr x) (cons #'name a))]
              [_ (f (cdr x) a)]))))
    (define (parse-opcode ilg)
      (lambda (oc)
        (with-syntax ((ilg ilg))
          (syntax-case oc (label newobj call callvirt)
            [(op class)
              (exists (lambda (x) (free-identifier=? #'op x)) 
                      (list #'isinst #'unbox.any #'box #'castclass #'newarr))
              #'(emit ilg (opcode op) (type class))]
            [(newobj type argtype ...)
              #'(emit ilg (opcode newobj) (ctor type argtype ...))]
            [(callvirt type argtype ...)
              #'(emit ilg (opcode callvirt) (method type argtype ...))]
            [(call type argtype ...)
              #'(emit ilg (opcode call) (method type argtype ...))]
            [(label name)
              #'(mark-label ilg name)]
            [(op arg)
              #'(emit ilg (opcode op) arg)]
            [op
              #'(emit ilg (opcode op))]))))
    (syntax-case x (=> locals)
      [(_ name (param-type ... => return-type) (locals (local-name local-type) ...) opcode ...)
        (with-syntax (((label ...) (get-labels #'(opcode ...))))
          #`(let* ((dm (make-dynamic-method 'name 
                                            (type return-type) 
                                            (type param-type) ... ))
                   (ilg (get-il-gen dm)))
               (let ((local-name (declare-local ilg (type local-type))) ...
                     (label (define-label ilg)) ...)
                 #,@(map (parse-opcode #'ilg) #'(opcode ...))
                 (make-proc dm))))]
      [(_ name (param-type ... => return-type) opcode ...)
        #'(il-method name 
                     (param-type ... => return-type) 
                     (locals) 
                     opcode ...)]
      [(_ (param-type ... => return-type) (locals (local-name local-type) ...) opcode ...)
        #'(il-method anon 
                     (param-type ... => return-type) 
                     (locals (local-name local-type) ...) 
                     opcode ...)]
      [(_ (param-type ... => return-type) opcode ...)
        #'(il-method (param-type ... => return-type) 
                     (locals) 
                     opcode ...)]
                 ))) 
            

(define cons+car 
  (il-method
    (System.Object System.Object System.Object => System.Object)
    (locals (foo System.Object))
    (ldarg 2)
    (castclass IronScheme.Runtime.ICallable)    
    (br hello)
    (label wooo)
    (ldarg 0)
    (ldarg 1)
    (starg 0)
    (starg 1)
    (br done)
    (label hello)
    ldarg.0
    ldarg.1
    (br wooo)
    (label done)
    (newobj IronScheme.Runtime.Cons System.Object System.Object)
    (stloc foo)
    (ldloc foo)
    dup
    (call IronScheme.Runtime.Builtins Car System.Object)
    tailcall
    (callvirt IronScheme.Runtime.ICallable Call System.Object System.Object)
    ret))

