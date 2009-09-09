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

(library (ironscheme clr dynamic)
  (export 
    clr-call-site
    clr-static-call-site
    clr-dynamic
    clr-cond
    else)
  (import
    (ironscheme)
    (ironscheme linq2)
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

  (define-syntax clr-cond-aux
    (generator #f))
    
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
                (property-set-value m instance '() (car args)))]
          [(field? m)
            (if (null? args)
                (field-get-value m instance)
                (field-set-value m instance (car args)))]))))
    
  (define-syntax clr-dynamic
    (lambda (x)
      (syntax-case x ()
        [(_ instance mem arg ...)
          #'(clr-dynamic-internal instance 'mem arg ...)])))   
          
  (define (find-clr-type namespaces type)
    (let ((t (symbol->string type)))
      (exists get-clr-type 
              (cons type 
                    (map (lambda (ns) 
                           (string->symbol 
                              (string-append ns "." t)))
                         namespaces)))))
                         
  (define (generate-method-sig meth argtypes)
    (string-append meth "(" (string-join "," argtypes) ")"))
                         
  (define (type-name/namespace type)
    (let ((ns (type-namespace type)))
      (if (zero? (string-length ns))
          (type-name type)
          (string-append ns "." (type-name type)))))
          
  (define (prepare-sigs mems)
    (iterator->list
      (from m in mems 
       let p = (method-params m)
       let l = (length p)
       group p by l into g
       orderby (key g)
       select (cons (key g) 
                    (iterator->list 
                      (from m in g 
                       select (iterator->list 
                                (from p in m 
                                 select (type-name/namespace (param-type p))))))))))
                                 
  (define (make-ids n)
    (map syntax->datum (generate-temporaries (make-list n))))   
    
  (define (generate-clauses type meth ids clauses static?)
    (let ((t (string->symbol type)))
      `(clr-cond ,(if static? ids (cons 'this ids))
         ,@(map (lambda (cls)
                  `[,(if static?
                         (map string->symbol cls)
                         (cons t (map string->symbol cls)))
                    ,(if static?
                         `(clr-static-call ,t ,(generate-method-sig meth cls) ,@ids)
                         `(clr-call ,t ,(generate-method-sig meth cls) this ,@ids))])
                clauses)
         (else (assertion-violation ,meth "failed to match arguments")))))
                         
  (define (generate-clr-call-site namespaces type meth static?)
    (let ((t (find-clr-type namespaces type)))
      (unless t
        (assertion-violation 'clr-call-site "type not found" type))
      (let* ((meth-name (symbol->string meth))
             (mems (type-member t meth-name 'method (list 'public (if static? 'static 'instance))))
             (sigs (prepare-sigs mems)))
        (eval (cons 'case-lambda
                    (map (lambda (sig) 
                           (let* ((ids (make-ids (car sig)))
                                  (cls (generate-clauses (type-name/namespace t) 
                                                         meth-name 
                                                         ids 
                                                         (cdr sig) 
                                                         static?)))
                            (if static?
                                `[(,@ids) ,cls]                                  
                                `[(this ,@ids) ,cls])))
                         sigs))
              (environment '(ironscheme) '(ironscheme clr) '(ironscheme clr dynamic))))))
    
  (define-syntax clr-call-site
    (syntax-rules ()
      [(_ type meth)
        (generate-clr-call-site (clr-namespaces) 'type 'meth #f)]))
        
  (define-syntax clr-static-call-site
    (syntax-rules ()
      [(_ type meth)
        (generate-clr-call-site (clr-namespaces) 'type 'meth #t)])))
    

          
          

