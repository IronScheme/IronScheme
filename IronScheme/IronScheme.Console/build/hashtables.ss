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
    (rnrs control)
    (only 
      (rnrs hashtables) 
      make-hashtable 
      hashtable-equivalence-function 
      hashtable-hash-function)
    (ironscheme core)
    (ironscheme clr))
    
  (clr-using system)
  (clr-using system.collections)
  
  (define (hashtable? obj)
    (clr-is Hashtable obj))
  
  (define make-eq-hashtable
    (case-lambda
      [()   (make-eq-hashtable 32)]
      [(k)  (clr-new hashtable (clr-cast int32 k))]))
    
  (define make-eqv-hashtable
    (case-lambda
      [()   (make-eqv-hashtable 32)]
      [(k)  (make-hashtable eqv-hash eqv? k)]))
  
  (define (hashtable-size ht)
    (clr-prop-get hashtable count ht))
    
  (define (hashtable-ref ht key default)
    (let ((r (clr-indexer-get hashtable ht key)))
      (if (or (not (null? r)) (hashtable-contains? ht key))
        r
        default)))
      
  (define (hashtable-set! ht key obj)
    (clr-indexer-set! hashtable ht key obj))

  (define (hashtable-delete! ht key)
    (clr-call hashtable remove ht key))
  
  (define (hashtable-contains? ht key)
    (clr-call hashtable containskey ht key))
    
  (define (hashtable-update! ht key proc default)
    (hashtable-set!
      ht key
      (proc (hashtable-ref ht key default))))

  (define hashtable-clear!
    (case-lambda 
      ((ht)     (hashtable-clear! ht 32))
      ((ht k)   (clr-call hashtable clear ht))))
      
  (define (string-hash str)
    (unless (string? str)
      (assertion-violation 'string-hash "not a string" str))
    (clr-call StringComparer "GetHashCode(String)"
        (clr-static-prop-get StringComparer Ordinal) 
        (clr-call Object ToString str)))

  (define (string-ci-hash str)
    (unless (string? str)
      (assertion-violation 'string-ci-hash "not a string" str))
    (clr-call StringComparer "GetHashCode(String)"
        (clr-static-prop-get StringComparer InvariantCultureIgnoreCase) 
        (clr-call Object ToString str)))
        
  (define (symbol-hash sym)
    (unless (symbol? sym)
      (assertion-violation 'symbol-hash "not a symbol" sym))        
    (clr-call Object GetHashCode sym))
    
  (define (equal-hash obj)
    (string-hash (format "~a" obj)))    
   

    
)