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
		(rnrs control)
    (ironscheme clr))
  
  (define (new) 
    (clr-new system.collections.hashtable))
    
  (define item
    (case-lambda
      [(ht key)         (clr-call system.collections.hashtable:get_item ht key)]
      [(ht key value)   (clr-call system.collections.hashtable:set_item ht key value)] ))

  (define (count ht)
    (clr-call system.collections.hashtable:get_count ht))

  (define (contains ht key)
    (clr-call system.collections.hashtable:contains ht key))
      
)