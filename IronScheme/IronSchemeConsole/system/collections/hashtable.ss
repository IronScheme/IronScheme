(library (system collections hashtable)
  (export
    ;; constructors
    new
    ;; indexer property
    item
    ;; read-only property
    count
    ;; method
    contains
    containskey
    )
    
  (import 
    (rnrs base)
		(rnrs syntax-case)
    (ironscheme clr))
    	        
  (define-syntax clr-method
    (syntax-rules ()
      [(_ type name args ...)
        (define-syntax name
          (syntax-rules ()
    	      [(_ instance args ...)  (clr-call type name instance args ...)]))]))
    
  (define-syntax type (identifier-syntax 'system.collections.hashtable))
    
  ;; how to deal with typed overloads?
  (define-syntax new
    (lambda (e)
		  (syntax-case e ()
      	[(_)              #`(clr-new #,type)]
      	[(_ k)            #`(clr-new #,type (clr-cast system.int32 k))] )))
    
  (define-syntax item
    (lambda (e)
      (syntax-case e ()
        [(_ ht key)       #`(clr-call #,type get_item ht key)]
        [(_ ht key value) #`(clr-call #,type set_item ht key value)] )))

  (define-syntax count
    (lambda (e)
      (syntax-case e ()
    	  [(_ ht)           #`(clr-call #,type get_count ht)])))

  (define-syntax contains
    (lambda (e)
      (syntax-case e ()
			  [(_  ht key)      #`(clr-call #,type contains ht key)])))
			  
  
   (clr-method system.collections.hashtable containskey key)		  
      
)
