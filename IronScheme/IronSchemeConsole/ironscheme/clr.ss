(library (ironscheme clr)
  (export
    clr-clear-usings
    clr-using
    clr-reference
    clr-is
    clr-foreach
    clr-cast
    clr-call
    clr-static-call
    clr-field-get
    clr-field-set!
    clr-static-field-get
    clr-static-field-set!
    clr-prop-get
    clr-prop-set!
    clr-static-prop-get
    clr-static-prop-set!
		clr-indexer-get
		clr-indexer-set!
    clr-new
		clr-new-array)
  (import 
    (rnrs)
    (only (psyntax system $bootstrap) gensym)
    (ironscheme clr internal))
    
  (define-syntax clr-clear-usings
    (lambda (e)
      (syntax-case e ()
        [(_)
         #`(define #,(gensym 'clear-usings) (clr-clear-usings-internal))])))      
    
  (define-syntax clr-using
    (lambda (e)
      (syntax-case e ()
        [(_ namespace)
         #`(define #,(gensym 'using) (clr-using-internal 'namespace))])))      

  (define-syntax clr-reference
    (lambda (e)
      (syntax-case e ()
        [(_ assname)
         #`(define #,(gensym 'reference) (clr-reference-internal 'assname))])))       
    
  (define-syntax clr-is
    (lambda (e)
      (syntax-case e ()
        [(_ type arg)
         #'(clr-is-internal 'type arg)])))    
  
  (define-syntax clr-call
    (lambda (e)
      (syntax-case e ()
        [(_ type member instance args ...)
         #'(clr-call-internal 'type 'member instance args ...)])))

  (define-syntax clr-field-get
    (lambda (e)
      (syntax-case e ()
        [(_ type member instance)
         #'(clr-field-get-internal 'type 'member instance)])))         
         
  (define-syntax clr-field-set!
    (lambda (e)
      (syntax-case e ()
        [(_ type member instance value)
         #'(clr-field-set!-internal 'type 'member instance value)])))         

  (define-syntax clr-static-field-get
    (syntax-rules ()
      [(_ type member)
        (clr-field-get type member '())]))

  (define-syntax clr-static-field-set!
    (syntax-rules ()
      [(_ type member value)
        (clr-field-set! type member '() value)]))
         
  (define-syntax clr-static-call
    (syntax-rules ()
      [(_ type member args ...)
        (clr-call type member '() args ...)]))
				
  (define-syntax clr-prop-get
    (lambda (e)
      (syntax-case e ()
        [(_ type prop-name instance args ...)
       		#`(clr-call-internal 'type 
							'#,(string->symbol (string-append "get_" (symbol->string (syntax->datum #'prop-name)))) 
							 instance args ...)])))
		
  (define-syntax clr-prop-set!
    (lambda (e)
      (syntax-case e ()
        [(_ type prop-name instance args ...)
       		#`(clr-call-internal 'type 
						  '#,(string->symbol (string-append "set_" (symbol->string (syntax->datum #'prop-name)))) 
						  instance args ...)])))		
       		
	(define-syntax clr-indexer-get
		(syntax-rules ()
			[(_ type instance arg args* ...)(clr-prop-get type item instance arg args* ...)]))

	(define-syntax clr-indexer-set!
		(syntax-rules ()
			[(_ type instance arg args* ... value)(clr-prop-set! type item instance arg args* ... value)]))
					
	(define-syntax clr-static-prop-get
		(syntax-rules ()
			[(_ type prop-name args ...)(clr-prop-get type prop-name '() args ...)]))
								
	(define-syntax clr-static-prop-set!
		(syntax-rules ()
			[(_ type prop-name args ...)(clr-prop-set! type prop-name '() args ...)]))

  (define-syntax clr-new
    (lambda (e)
      (syntax-case e ()
        [(_ type args ...)
         #'(clr-new-internal 'type args ...)])))
				
  (define-syntax clr-new-array
    (lambda (e)
      (syntax-case e ()
        [(_ type size)
         #'(clr-new-array-internal 'type size)])))				
         
  (define-syntax clr-cast
    (lambda (e)
      (syntax-case e ()
        [(_ type arg)
         #'(clr-cast-internal 'type arg)])))
  
  ;; little helper for C# people     
  (define-syntax clr-foreach
    (syntax-rules (in)
      ((_ e in lst body body* ...)
       (for-each (lambda (e) body body* ...) lst))))                  
         
)
    
