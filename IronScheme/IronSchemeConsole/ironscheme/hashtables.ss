(library (ironscheme hashtables)

  (export
    make-eq-hashtable
    make-eqv-hashtable
    
    hashtable-size
    hashtable-ref
    hashtable-set!
    hashtable-delete!
    hashtable-contains?
    hashtable-update!
    hashtable-clear!
    
    ;; todo
    hashtable-equivalence-function
    hashtable-hash-function)
    
  (import 
    (rnrs base)
    (rnrs control)
    (ironscheme clr))
  
  (define make-eq-hashtable
    (case-lambda
      [()   (clr-new system.collections.hashtable)]
      [(k)  (clr-new system.collections.hashtable (clr-cast system.int32 k))]))
    
  (define make-eqv-hashtable
    (case-lambda
      [()   (clr-new system.collections.hashtable)]
      [(k)  (clr-new system.collections.hashtable (clr-cast system.int32 k))]))
  
  (define (hashtable-size ht)
    (clr-call system.collections.hashtable get_count ht))
    
  (define (hashtable-ref ht key default)
    (define r (clr-call system.collections.hashtable get_item ht key))
    
    (if (not (null? r)) 
      r
      default))
      
  (define (hashtable-set! ht key obj)
    (clr-call system.collections.hashtable set_item ht key obj))

  (define (hashtable-delete! ht key)
    (clr-call system.collections.hashtable remove ht key))
  
  (define (hashtable-contains? ht key)
    (clr-call system.collections.hashtable containskey ht key))
    
  (define (hashtable-update! ht key proc default)
    (hashtable-set!
      ht key
      (proc (hashtable-ref ht key default))))

  (define hashtable-clear!
    (case-lambda 
      ((ht)     (clr-call system.collections.hashtable clear ht))
      ((ht k)   (clr-call system.collections.hashtable clear ht))))
    
  ;; TODO
  
  (define hashtable-equivalence-function #f)
  (define hashtable-hash-function #f)
  
  ;(define make-hashtable  #f)    ; internal
  ;(define hashtable?      #f)    ; internal
  ;(define hashtable-copy    #f)  ; internal
  ;(define hashtable-keys    #f)  ; internal
  ;(define hashtable-entries #f)  ; internal
  ;(define hashtable-mutable? #f) ; internal
  ;(define equal-hash #f)         ; internal
  ;(define string-hash #f)        ; internal
  ;(define string-ci-hash #f)     ; internal
  ;(define symbol-hash #f)        ; internal
)