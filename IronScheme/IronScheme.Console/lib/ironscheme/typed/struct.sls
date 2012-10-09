#| License
Copyright (c) 2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme typed struct)
  (export
    :
    ->
    struct:
    import-struct-type
    struct-type-descriptor
    struct-constructor-descriptor
    struct-mutator
    struct-accessor
    struct-method
    struct-predicate
    struct-constructor
    define-method)
  (import 
    (ironscheme)
    (ironscheme typed core)
    (ironscheme typed parsing)
    (ironscheme typed struct-descriptor)
    (ironscheme clr) 
    (ironscheme syntax))
    
  (define-syntax import-struct-type
    (lambda (x)
      (syntax-case x ()
        [(_ type)
          (lambda (lookup)
            (let ((r (lookup #'type)))
              (unless r
                (syntax-violation 'import-struct-type "not a valid type" #'type))
              (with-syntax ((r (datum->syntax #'type (struct-descriptor-namespace r))))
                #'(clr-using r))))])))
                
  (define-syntax define-member-accessor
    (syntax-rules ()
      [(_ name accessor access-type)
        (define-syntax name
          (lambda (x)
            (syntax-case x ()
              [(_ type member)
                (lambda (lookup)
                  (let ((r (lookup #'type)))
                    (unless r
                      (syntax-violation 'name "not a valid type" #'type))
                    (let* ((m (accessor r))
                           (f (assq (syntax->datum #'member) m)))
                      (unless f
                        (syntax-violation 'name (string-append "not a valid " access-type) #'type #'member))
                      (cdr f))))])))]))                        
  
  (define-member-accessor struct-mutator   struct-descriptor-mutators  "field")
  (define-member-accessor struct-accessor  struct-descriptor-accessors "field")
  (define-member-accessor struct-method    struct-descriptor-methods   "method")
                
  (define-syntax define-accessor 
    (syntax-rules ()
      [(_ name accessor)
        (define-syntax name
          (lambda (x)
            (syntax-case x ()
              [(_ type)
                (lambda (lookup)
                  (let ((r (lookup #'type)))
                    (unless r
                      (syntax-violation 'name "not a valid type" #'type))
                    (accessor r)))])))]))
  
  (define-accessor struct-predicate               struct-descriptor-predicate)
  (define-accessor struct-constructor             struct-descriptor-ctor)
  (define-accessor struct-type-descriptor         struct-descriptor-rtd)
  (define-accessor struct-constructor-descriptor  struct-descriptor-rcd)

  (define-syntax define-method
    (lambda (x)
      (syntax-case x ()
        [(_ type (name args ...) b b* ...)
          (lambda (lookup)
            (let* ((r (lookup #'type))
                   (flds (map (lambda (f)
                                (datum->syntax #'type f))
                              (struct-descriptor-field-names r)))
                   (mths (map (lambda (m)
                                (datum->syntax #'type m))
                              (struct-descriptor-method-names r))))
              (with-syntax ([((args args-type) ...) (map parse-arg-type #'(args ...))]
                            [ (ret-type b b* ...) (parse-return-type-body #'(b b* ...))])
                (with-syntax ((this (datum->syntax #'name 'this))
                              ((type-fld ...) flds)
                              ((type-meth ...) mths)
                              ((meth ...)     (map (lambda (m)
                                                     (list #'struct-method #'type m))
                                                   mths))
                              ((type-get ...) (map (lambda (f)
                                                     (list #'struct-accessor #'type f))
                                                   flds))
                              ((type-set ...) (map (lambda (f)
                                                     (list #'struct-mutator #'type f))
                                                   flds))
                              ((xargs ...)    (generate-temporaries #'(args ...))))
                  #'(define: (name (this : type) (xargs : args-type) ...) : ret-type
                      (when (null? this)
                        (assertion-violation 'name "instance cannot be null"))
                      (let-syntax ((type-fld (identifier-syntax [_ (type-get this)]
                                                                [(set! _ val) (type-set this val)])) ...
                                   (type-meth (lambda (x)
                                                (syntax-case x ()
                                                  [(_ . arg) #'(meth this . arg)]
                                                  [_ #'(lambda x (apply meth this x))]))) ... )
                        (let-syntax ((args (identifier-syntax [_ xargs]
                                                              [(set! _ val) (set! xargs val)])) ...
                                     (this (lambda (x)
                                             (symbolic-case x (type-fld ...)
                                              [(_ type-fld) #'type-fld] ...
                                              [_ #'this]))))
                          b b* ...)))))))])))              
    
  (define-syntax struct:
    (lambda (x)
      (define (iota n)
        (let f ((i 0)(a '()))
          (if (= i n)
              (reverse a)
              (f (+ i 1) (cons i a)))))
      (define (parse-method type)
        (lambda (m)
          (syntax-case m ()
            [(name (arg ...) b b* ...)
              (with-syntax ((n (syntax-format "~a-~a" type type #'name)))
                #'(n name (arg ...) b b* ...))])))
      (syntax-case x ()
        [(_ name (field ...) method ...)
          (with-syntax ([((fldname fldtype) ...) 
                          (map parse-arg-type #'(field ...))]
                        [((method-name method-name-short (method-arg ...) method-body ...) ...)
                          (map (parse-method #'name) #'(method ...))])
            (with-syntax ((make (syntax-format "make-~a" #'name #'name))
                          (pred (syntax-format "~a?" #'name #'name))
                          (((getter getter-i) ...)
                            (let ((flds #'(fldname ...)))
                              (map (lambda (g i)
                                     (list (syntax-format "~a-~a" #'name #'name g) i))
                                   flds
                                   (iota (length flds)))))
                          (((setter setter-i) ...)
                            (let ((flds #'(fldname ...)))
                              (map (lambda (s i)
                                     (list (syntax-format "set-~a-~a!" #'name #'name s) i))
                                   flds
                                   (iota (length flds))))))
              (let* ((uid (gensym))
                     (suid (datum->syntax #'name uid))
                     (ns (datum->syntax #'name (string->symbol (format "record.~a" uid)))))
                #`(begin
                    (define rtd (make-record-type-descriptor 
                                  'name #f '#,suid #f #f 
                                  '#((mutable fldname) ...)
                                  '#(fldtype ...))) ; using extended version
                    (define rcd (make-record-constructor-descriptor rtd #f #f))
                    (define make (record-constructor rcd))
                    (define pred (record-predicate rtd))
                    (define getter (record-accessor rtd getter-i)) ...
                    (define setter (record-mutator rtd setter-i)) ...
                    
                    (define-syntax name 
                      (make-compile-time-value 
                        (make-struct-descriptor 'name '#,ns 
                                                #'rtd #'rcd 
                                                #'make 
                                                #'pred 
                                                '(fldname ...)
                                                '((fldname . fldtype) ...)
                                                (list (cons 'fldname #'getter) ...)
                                                (list (cons 'fldname #'setter) ...)
                                                (list (cons 'method-name-short #'method-name) ...))))

                    (clr-using #,ns)
                    
                    (define-method name (method-name method-arg ...) method-body ...) ... ))))]))))