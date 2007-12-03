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
    )
    
  (import 
    (rnrs base)
		(rnrs syntax-case)
    (ironscheme clr))
    
  ;; how to deal with typed overloads?
  (define-syntax new
		(syntax-rules ()
    	[(_)                (clr-new system.collections.hashtable)]
    	[(_ k)              (clr-new system.collections.hashtable (clr-cast system.int32 k))]))
    
  (define-syntax item
    (syntax-rules ()
      [(_ ht key)         (clr-call system.collections.hashtable:get_item ht key)]
      [(_ ht key value)   (clr-call system.collections.hashtable:set_item ht key value)] ))

  (define-syntax count
		(syntax-rules ()
    	[(_ ht)             (clr-call system.collections.hashtable:get_count ht)]))

  (define-syntax contains
		(syntax-rules ()
			[(_  ht key)        (clr-call system.collections.hashtable:contains ht key)]))
      
)
