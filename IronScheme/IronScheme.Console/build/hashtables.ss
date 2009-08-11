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

(library (ironscheme hashtables)
  (export
    make-eq-hashtable
    make-eqv-hashtable
    hashtable?
    
    hashtable-size
    hashtable-ref
    hashtable-set!
    hashtable-delete!
    hashtable-contains?
    hashtable-update!
    hashtable-clear!
    
    string-hash
    string-ci-hash
    symbol-hash
    equal-hash

    hashtable-equivalence-function
    hashtable-hash-function)
    
  (import 
    (rnrs base)
    (rnrs arithmetic fixnums)
    (rnrs control)
    (only 
      (rnrs hashtables) 
      make-hashtable 
      hashtable-equivalence-function 
      hashtable-hash-function)
    (ironscheme core)
    (ironscheme contracts)
    (ironscheme clr))
    
  (clr-using system)
  (clr-using system.collections)
  
  (define (hashtable? obj)
    (clr-is Hashtable obj))
  
  (define/contract make-eq-hashtable
    (case-lambda
      [()           (make-eq-hashtable 32)]
      [(k:fixnum)   (clr-new hashtable (clr-cast int32 k))]))
    
  (define/contract make-eqv-hashtable
    (case-lambda
      [()           (make-eqv-hashtable 32)]
      [(k:fixnum)   (make-hashtable eqv-hash eqv? k)]))
  
  (define/contract (hashtable-size ht:hashtable)
    (clr-prop-get hashtable count ht))
    
  (define/contract (hashtable-ref ht:hashtable key default)
    (let ((r (clr-indexer-get hashtable ht key)))
      (if (or (not (null? r)) (hashtable-contains? ht key))
        r
        default)))
      
  (define/contract (hashtable-set! ht:hashtable key obj)
    (clr-indexer-set! hashtable ht key obj))

  (define/contract (hashtable-delete! ht:hashtable key)
    (clr-call hashtable remove ht key))
  
  (define/contract (hashtable-contains? ht:hashtable key)
    (clr-call hashtable containskey ht key))
    
  (define/contract (hashtable-update! ht:hashtable key proc:procedure default)
    (hashtable-set!
      ht key
      (proc (hashtable-ref ht key default))))

  (define/contract hashtable-clear!
    (case-lambda 
      [(ht:hashtable)     
        (hashtable-clear! ht 32)]
      [(ht:hashtable k:fixnum)   
        (clr-call hashtable clear ht)]))
      
  (define/contract (string-hash str:string)
    (clr-call StringComparer "GetHashCode(String)"
        (clr-static-prop-get StringComparer Ordinal) 
        (clr-call Object ToString str)))

  (define/contract (string-ci-hash str:string)
    (clr-call StringComparer "GetHashCode(String)"
        (clr-static-prop-get StringComparer InvariantCultureIgnoreCase) 
        (clr-call Object ToString str)))
        
  (define/contract (symbol-hash sym:symbol)
    (clr-call Object GetHashCode sym))
    
  (define (equal-hash obj)
    (string-hash (format "~a" obj)))    
   

    
)