(library (source-optimizer primitive)
  (export
    foldable?
    effect-free?
    primitive-info
    system-value
    result-true?
    result-false?
    primitive?)
  (import 
    (rnrs)
    (source-optimizer prelex)
    (source-optimizer match))
    
  (define-record-type info 
    (fields foldable? effect-free? result-true? result-false? value))    
    
  (define-syntax primitives
    (lambda (x)
      (define (quote-head form)
        (syntax-case form ()
          [(head . rest)
            #'('head . rest)]))
      (define (get-head form)
        (syntax-case form ()
          [(head . rest) #'head]))
      (syntax-case x ()
        [(_ e [form foldable? effect-free? result-true? result-false?] ...)
          (with-syntax (((form ...) (map quote-head #'(form ...)))
                        ((head ...) (map get-head #'(form ...))))
            #'(if (eq? (cadr e) check)
                  (match (car e)
                    ['head #t] ...
                    [_ #f])
                  (match e
                      [form (make-info foldable?
                                       effect-free?
                                       result-true?
                                       result-false?
                                       head)] ...
                      [_ #f])))])))

  (define check (list 'check))    
  
  (define (primitive? p)
    (primitive-info p check))
  
  (define (primitive-info p opnd)
    (primitives (list p opnd)
      ;template       foldable?  effect-free?  result-true? result-false?
      [(not _)        #t         #t            #f           #f          ]
      [(+ _ . _)      #t         #t            #t           #f          ]
      [(- _ . _)      #f         #t            #t           #f          ]
      [(* _ . _)      #f         #f            #t           #f          ]))
          
  (define (foldable? p)
    (and (info? p)
         (info-foldable? p)))
                
  (define (effect-free? p)
    (and (info? p)
         (info-effect-free? p)))
           
  (define (result-true? p) 
    (and (info? p)
         (info-result-true? p)))
               
  (define (result-false? p)
    (and (info? p)
         (info-result-false? p)))  
  
  (define (system-value p)
    (unless (info? p)
      (assertion-violation 'system-value "unknown primitive" p))
    (info-value p)))