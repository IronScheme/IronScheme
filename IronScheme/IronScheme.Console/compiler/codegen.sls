(library (compiler codegen)
  (export opcode?)
  (import 
    (ironscheme)
    (ironscheme linq)
    (ironscheme clr)
    (ironscheme clr dynamic)
    (ironscheme strings)
    (ironscheme contracts)
    (ironscheme clr reflection))

  (clr-using System.Reflection)
  (clr-using System.Reflection.Emit)  

  (define mb (clr-static-prop-get IronScheme.Runtime.Builtins ModuleBuilder))

  (define/contract (make-dynamic-method name return-type:clr-type . param-types:clr-type)
    (clr-new DynamicMethod (symbol->string name) return-type (list->vector param-types)))

  (define/contract (make-il-method name return-type:clr-type . param-types:clr-type)
    (clr-static-call IronScheme.Runtime.Builtins MakeMethod (symbol->string name) return-type (list->vector param-types)))
        
  (define (closure-targets proc)
    (clr-prop-get IronScheme.Runtime.Closure Targets proc))
    
  (define (closure-target proc)
    (let ((targets (clr-prop-get IronScheme.Runtime.Closure Targets proc)))
      (when (zero? (vector-length targets))
        (assertion-violation 'closure-target "no targets" proc))
      (vector-ref targets 0)))
        
  (define (map->clr-types types)
    (map (lambda (x)
           (if (list? x)
               (apply get-clr-type (car x) (map->clr-types (cdr x)))
               (get-clr-type x)))
         types))      
        
  (define (map-clr-types return-type arg-types)
     (map->clr-types (append arg-types (list return-type))))
        
  (define (get-typed-delegate-type return-type . arg-types)
    (apply get-clr-type 'IronScheme.Runtime.Func (map-clr-types return-type arg-types)))      

  (define (make-proc dm return-type . arg-types)
    (let ((delegate (clr-call DynamicMethod CreateDelegate dm (apply get-typed-delegate-type return-type arg-types))))
      (create-instance (apply get-clr-type 'IronScheme.Runtime.TypedClosure (map-clr-types return-type arg-types)) 
                       delegate)))

  (define (make-il-proc dm return-type . arg-types)
    (let ((delegate (clr-static-call IronScheme.Runtime.Builtins CreateDelegate dm (apply get-typed-delegate-type return-type arg-types))))
      (create-instance (apply get-clr-type 'IronScheme.Runtime.TypedClosure (map-clr-types return-type arg-types)) 
                       delegate)))
                       
  (define (opcode? obj)
    (clr-is OpCode obj))
    
  (define (get-il-gen dm)
    (cond
      [(clr-is DynamicMethod dm)  (clr-call DynamicMethod GetILGenerator dm)]
      [(clr-is MethodBuilder dm)  (clr-call MethodBuilder GetILGenerator dm)]
      [(clr-is ConstructorBuilder dm) (clr-call ConstructorBuilder GetILGenerator dm)]
      [else (assertion-violation 'get-il-gen "not supported" dm)]))
    
  (define (create-instance type . args)
    (clr-static-call Activator (CreateInstance Type Object[]) type (list->vector args)))    

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
            
  (define (current-app-domain) 
    (clr-static-prop-get System.AppDomain CurrentDomain))

  (define (make-assembly-name name)
    (clr-new AssemblyName name))
            
  (define create-delegate (clr-static-call-site System.Delegate CreateDelegate))

  (define make-assembly (clr-call-site System.AppDomain DefineDynamicAssembly))

  (define make-module (clr-call-site System.Reflection.Emit.AssemblyBuilder DefineDynamicModule))
  (define save-assembly (clr-call-site System.Reflection.Emit.AssemblyBuilder Save))
            
  (define make-type (clr-call-site System.Reflection.Emit.ModuleBuilder DefineType))

  (define make-field  (clr-call-site System.Reflection.Emit.TypeBuilder DefineField))
  (define make-method (clr-call-site System.Reflection.Emit.TypeBuilder DefineMethod))
  (define make-nested-type (clr-call-site System.Reflection.Emit.TypeBuilder DefineNestedType))
  (define make-constructor (clr-call-site System.Reflection.Emit.TypeBuilder DefineConstructor))
  (define make-property (clr-call-site System.Reflection.Emit.TypeBuilder DefineProperty))
  (define make-type-initializer (clr-call-site System.Reflection.Emit.TypeBuilder DefineTypeInitializer))

  (define set-generic-type-parameters (clr-call-site System.Reflection.Emit.TypeBuilder DefineGenericParameters))
  (define set-generic-method-parameters (clr-call-site System.Reflection.Emit.MethodBuilder DefineGenericParameters))

  (define create-type (clr-call-site System.Reflection.Emit.TypeBuilder CreateType))


  (define-syntax il-member
    (lambda (x)
      (syntax-case x ()
        [(_ tb mem)
          (syntax-case #'mem (field method property event type)
            [(field name attr fieldtype) 
              #'(make-field tb (symbol->string 'name) (type fieldtype) 'attr)]
            [(method name attr (param-type ... => return-type) body ... )
              #'(let* ((name (make-method tb (symbol->string 'name) 'attr (type return-type) (vector (type param-type) ...)))
                       (ilg  (get-il-gen name)))
                  (il-method-body ilg body ...)
                  name)]
            [(ctor attr (param-type ...) body ...)
              #'(let* ((name (make-constructor tb 'attr 'standard (vector (type param-type) ...)))
                       (ilg  (get-il-gen name)))
                  (il-method-body ilg body ...))])])))

  (define-syntax il-type
    (lambda (x)
      (syntax-case x (type)
        [(_ mb (type name attr mem ...))
          #'(let ((name (make-type mb (symbol->string 'name) 'attr)))
              (il-member name mem) ...
              (create-type name))])))

  (define-syntax ilasm
    (lambda (x)
      (define (md-type x)
        (syntax-case x (type)
          [(_ mb (type name attr mem ...))
            #'(name (make-type mb (symbol->string 'name) 'attr))]))
      (syntax-case x (assembly module)
        [(_ (assembly assname)
            (module modname
              type ...))
          #'(let* ((assname (make-assembly (current-app-domain) (make-assembly-name (symbol->string 'assname)) 'save))
                   (modname (make-module assname (symbol->string 'modname))))
              (il-type modname type) ...
              assname)])))

  (define emit (clr-call-site System.Reflection.Emit.ILGenerator Emit))        

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
        [(_ (class typearg ...))
          #'(get-clr-type 'class (type typearg) ...)]
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
          
  (define-syntax il-method-body
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
              [(callvirt type name argtype ...)
                #'(emit ilg (opcode callvirt) (method type name argtype ...))]
              [(call type name argtype ...)
                #'(emit ilg (opcode call) (method type name argtype ...))]
              [(label name)
                #'(mark-label ilg name)]
              [(op arg)
                #'(emit ilg (opcode op) arg)]
              [op
                #'(emit ilg (opcode op))]))))  
      (syntax-case x (locals)
        [(_ ilg (locals (local-name local-type) ...) opcode ...)
          (with-syntax (((label ...) (get-labels #'(opcode ...))))
            #`(let ((local-name (declare-local ilg (type local-type))) ...
                    (label (define-label ilg)) ...)
                #,@(map (parse-opcode #'ilg) #'(opcode ...))))])))
       
  (define-syntax il-method
    (lambda (x)
      (syntax-case x (=> locals)
        [(_ name (param-type ... => return-type) (locals (local-name local-type) ...) opcode ...)
          #`(let* ((dm (make-il-method 'name 
                                       (type return-type) 
                                       (type param-type) ... ))
                   (ilg (get-il-gen dm)))
               (il-method-body ilg (locals (local-name local-type) ...) opcode ...)   
               (make-il-proc dm 'return-type 'param-type ...))]
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
              
  #|
  (define cons+car 
    (il-method
      (System.Object System.Object System.Object => System.Object)
      (locals (foo System.Object))
      (ldarg 2)
      (castclass IronScheme.Runtime.Callable)    
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
      (callvirt IronScheme.Runtime.Callable Call System.Object System.Object)
      ret))
      
  (define my+ 
    (il-method
      (System.Int32 System.Int32 => System.Int32)
      ldarg.0
      ldarg.1
      add
      ret)) 
      
  (define myfoo
    (il-method
      (System.Int32 (IronScheme.Runtime.ITypedCallable System.Int32 System.Int32 System.Int32) => System.Int32)
      (locals (counter System.Int32))
      
      (label loop)
      (ldloc counter)
      (ldarg 0)
      clt
      (brfalse done)
      (ldarg 1)
      (ldloc counter)
      (ldc.i4 1)
      (callvirt (IronScheme.Runtime.ITypedCallable System.Int32 System.Int32 System.Int32) Invoke System.Int32 System.Int32)
      (stloc counter)
      (br loop)
      
      (label done)
      (ldloc counter)
      ret))
        
  (define mybar
    (il-method
      (System.Int32 => System.Int32)
      (locals (counter System.Int32))
      
      (label loop)
      (ldloc counter)
      (ldarg 0)
      clt
      (brfalse done)
      (ldloc counter)
      (ldc.i4 1)
      (call my+)
      (stloc counter)
      (br loop)

      (label done)
      (ldloc counter)
      ret))      

   ;(time (let f ((i 0)) (if (fx<? i 10000000) (f (fx+ i 1)) i)))
   ;(time (myfoo 10000000 my+))
   ;(time (mybar 10000000))
 
   
  (define myhello
    (ilasm
      (assembly myhello)
      (module myhello.dll
        (type foo public
          (method bar (public static) (System.Int32 => System.Int32)
            (locals)
            ldarg.0
            ret)
          (method baz (public static) (=> System.Int32)
            (locals)
            (ldc.i4 1)
            ret)
            ))))
    |#          
  ;(save-assembly myhello "myhello.dll")
  
)