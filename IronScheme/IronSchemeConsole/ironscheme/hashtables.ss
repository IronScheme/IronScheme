(library (ironscheme hashtables (6))

  (export
    make-eq-hashtable
    make-eqv-hashtable
    make-hashtable
    
    hashtable?
    hashtable-size
    hashtable-ref
    hashtable-set!
    hashtable-delete!
    hashtable-contains?
    hashtable-update!
    hashtable-copy
    hashtable-clear!
    hashtable-keys
    hashtable-entries
    
    hashtable-equivalence-function
    hashtable-hash-function
    hashtable-mutable?
    
    equal-hash
    string-hash
    string-ci-hash
    symbol-hash)
    
  (import 
    (rnrs base)
    (rnrs control)
    (ironscheme clr))
  
  
  (define make-eq-hashtable
    (case-lambda
      [()   (clr-new system.collections.hashtable)]
      [(k)  (clr-new system.collections.hashtable k)]))
    
  (define make-eqv-hashtable
    (case-lambda
      [()   (clr-new system.collections.hashtable)]
      [(k)  (clr-new system.collections.hashtable k)]))
    
  (define make-hashtable  #f)
  (define hashtable?      #f)
  
  (define (hashtable-size ht)
    (clr-call system.collections.hashtable:get_count ht))
    
  (define (hashtable-ref ht key default)
    (define r (clr-call system.collections.hashtable:get_item ht key))
    
    (if (not (null? r)) 
      r
      default))
      
  (define (hashtable-set! ht key obj)
    (clr-call system.collections.hashtable:set_item ht key obj))

  (define (hashtable-delete! ht key)
    (clr-call system.collections.hashtable:remove ht key))
  
  (define (hashtable-contains? ht key)
    (clr-call system.collections.hashtable:containskey ht key))
    
  (define hashtable-update! #f)
  (define hashtable-copy    #f)
  
  (define (hashtable-clear! ht)
    (clr-call system.collections.hashtable:clear ht))
    
  (define hashtable-keys    #f)
  (define hashtable-entries #f)
  
  (define hashtable-equivalence-function #f)
  (define hashtable-hash-function #f)
  (define hashtable-mutable? #f)
    
  (define equal-hash #f)
  (define string-hash #f)
  (define string-ci-hash #f)
  (define symbol-hash #f)
)