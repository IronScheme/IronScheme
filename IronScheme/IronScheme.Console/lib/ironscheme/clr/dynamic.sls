#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme clr dynamic)
  (export 
    clr-call-site
    clr-static-call-site
    clr-dynamic
    clr-cond
    else)
  (import
    (ironscheme)
    (ironscheme linq)
    (ironscheme strings)
    (ironscheme clr)
    (ironscheme unsafe)
    (ironscheme clr reflection)
    (ironscheme fsm-cond-helpers))
  
  (clr-using System.Reflection)    

  (define-syntax clr-cond
    (lambda (x)
      (define (gen-predicate pred id)
        (list pred
              id
              (let ((x (symbol->string (syntax->datum pred))))
                (if (string-ends-with? x "?")
                    (let ((p (string->symbol (substring x 0 (- (string-length x) 1)))))
                      #`($or? (clr-is #,(datum->syntax pred p) #,id) (null? #,id)))
                    #`(clr-is #,pred #,id)))))
      (syntax-case x (else)
        [(_ (id ...) ((pred ...) expr) ... (else else-expr))
          (let ((preds (vector->list (get-predicates #'(pred ... ...)))))
            (with-syntax ((((pred* x test) ...) (map gen-predicate 
                                                     preds 
                                                     (generate-temporaries preds))))
              #'(let-syntax
                    ((pred* (syntax-rules () [(_ x) test])) ...)
                  (clr-cond-aux (id ...)
                                ((pred ...) expr) ...
                                (else else-expr)))))]
        [(_ (id ...) ((pred ...) expr) ...)
          #'(clr-cond (id ...) ((pred ...) expr) ... (else #f))])))

  (define-syntax clr-cond-aux (fsm-cond-transformer #f))
    
  (define (clr-call-targets type meth argtypes)
    (let ((len  (length argtypes))
          (meth (symbol->string meth)))
      (find-type-members type 
                         '(method property field)
                         '(public instance)
                         (lambda (m c)
                          (and (string=? (member-name m) meth)
                               (cond
                                [(method? m)
                                   (let ((p (method-params m)))
                                    (and (fx=? len (length p))
                                         (for-all type-assignable-from?
                                                  (map param-type p)
                                                  argtypes)))]
                                [(property? m)
                                  (case len
                                    [(0) #t]
                                    [(1) (type-assignable-from? (property-type m) 
                                                                (car argtypes))]
                                    [else #f])]
                                [(field? m)
                                  (case len
                                    [(0) #t]
                                    [(1) (type-assignable-from? (field-type m) 
                                                                (car argtypes))]
                                    [else #f])]                                    
                                [else #f])))
                         #f)))
    
  (define (clr-dynamic-internal instance mem . args)
    (let* ((t (typeof instance))
           (m (clr-call-targets t mem (map typeof args))))
      (when (null? m)
        (assertion-violation 'clr-dynamic "member not found on type" t mem))
      (let ((m (car m)))
        (cond
          [(method? m)        
            (method-invoke m instance args)]
          [(property? m)
            (if (null? args)
                (property-get-value m instance '())
                (begin (property-set-value m instance '() (car args))
                       (void)))]
          [(field? m)
            (if (null? args)
                (field-get-value m instance)
                (begin (field-set-value m instance (car args))
                       (void)))]))))
    
  (define-syntax clr-dynamic
    (lambda (x)
      (syntax-case x ()
        [(_ instance mem arg ...)
          #'(clr-dynamic-internal instance 'mem arg ...)])))   
          
  (define (generate-method-sig meth argtypes)
    (cons (string->symbol meth) (map type-fullname argtypes)))

  (define (type-and-namespace type ns)
    (if (zero? (string-length ns))
        type
        (string-append ns "." type)))
    
  (define get-type
    (case-lambda
      [(name)
        (get-type name '())]
      [(name ns)
        (or (exists
              (lambda (ns)
                (let ((t (clr-static-call IronScheme.Runtime.Helpers GetTypeFast (type-and-namespace name ns))))
                  (if (null? t) #f t)))
              (cons "" ns))
            (assertion-violation 'get-type "type not found" name ns))]
      [(name ns . args)
        (let* ((gt  args)
               (len (length gt))
               (t   (get-type (string-append name "`" (number->string len)) ns)))
          (clr-call Type (MakeGenericType Type[]) t (list->vector gt)))]))               
            
  (define-syntax type
    (lambda (x)
      (syntax-case x ()
        [(_ (class typearg ...))
          #'(get-type (symbol->string 'class) (clr-namespaces) (type typearg) ...)]
        [(_ class)
          #'(type (class))])))                      
                         
          
  (define (prepare-sigs mems)
    (iterator->list
      (from m in mems
       where (not (method-contains-generic-parameters? m)) 
       let p = (method-params m)
       let l = (length p)
       group p by l into g
       orderby (key g)
       select (cons 
                (key g) 
                (iterator->list 
                  (from m in g 
                   select (iterator->list 
                            (from p in m 
                             select (param-type p)))))))))
                                 
  (define (make-ids n)
    (map syntax->datum (generate-temporaries (make-list n)))) 
    
  (define (generate-clause p)
    (cond
      [(type-enum? p)
        'Microsoft.Scripting.SymbolId]
      [(type-assignable-from? (get-type "System.Delegate") p)
        'IronScheme.Runtime.Callable]
      [(type-array? p)
        'System.Array]
      [else
        (let ((n (type-fullname p)))
          (string->symbol 
            (if (type-valuetype? p)
                n
                (string-append n "?"))))]))
            
  (define (wrap-clause t static? cls)
    (if static?
        cls
        `(if (clr-is ,t this)
             ,cls
             (assertion-violation 'clr-call-site "instance not matched" this ,t))))
    
  (define (generate-clauses type meth ids clauses static?)
    (let ((t (type-fullname type)))
      (wrap-clause t static?
        `(clr-cond ,ids
           ,@(map (lambda (cls)
                    `[,(map generate-clause cls)
                      ,(if static?
                           `(clr-static-call ,t ,(generate-method-sig meth cls) ,@ids)
                           `(clr-call ,t ,(generate-method-sig meth cls) this ,@ids))])
                  clauses)
           (else (assertion-violation ,(string-append t "::" meth) 
                                      "failed to match arguments"
                                      ,@ids))))))
                         
  (define (generate-clr-call-site type meth static?)
    (let* ((meth-name (symbol->string meth))
           (mems (type-member type meth-name 'method (list 'public (if static? 'static 'instance))))
           (sigs (prepare-sigs mems)))
      (when (null? sigs)
        (assertion-violation 'clr-call-site "no candidates found for method" meth type))
      (eval (cons 'case-lambda
                  (map (lambda (sig) 
                         (let* ((ids (make-ids (car sig)))
                                (cls (generate-clauses type
                                                       meth-name 
                                                       ids 
                                                       (cdr sig) 
                                                       static?)))
                          (if static?
                              `[(,@ids) ,cls]                                  
                              `[(this ,@ids) ,cls])))
                       sigs))
            (environment '(ironscheme) '(ironscheme clr) '(ironscheme clr dynamic)))))
              
            
    
  (define-syntax clr-call-site
    (syntax-rules ()
      [(_ clr-type meth)
        (generate-clr-call-site (type clr-type) 'meth #f)]))
        
  (define-syntax clr-static-call-site
    (syntax-rules ()
      [(_ clr-type meth)
        (generate-clr-call-site (type clr-type) 'meth #t)])))