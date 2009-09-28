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
    make-hashtable
    
    hashtable-copy
    hashtable-keys
    hashtable-mutable?
    eqv-hash
    
    
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
    (except (ironscheme core) eqv-hash)
    (ironscheme contracts)
    (ironscheme clr))
    
  (clr-using System.Collections)
  (clr-using IronScheme.Runtime.R6RS)
  
  (define (hashtable? obj)
    (clr-is Hashtable obj))
  
  (define/contract make-eq-hashtable
    (case-lambda
      [()           (make-eq-hashtable 32)]
      [(k:fixnum)   (clr-new Hashtable (clr-cast Int32 k))]))
    
  (define/contract make-eqv-hashtable
    (case-lambda
      [()           (make-eqv-hashtable 32)]
      [(k:fixnum)   (make-hashtable eqv-hash eqv? k)]))
      
  (define/contract make-hashtable
    (case-lambda
      [(hash equiv)
        (make-hashtable hash equiv 32)]
      [(hash:procedure equiv:procedure k:fixnum)
        (let ((cmp (clr-new HashComparer hash equiv)))
          (clr-new HashtableEx k cmp))]))  
          
  (define/contract hashtable-copy
    (case-lambda
      [(ht)
        (hashtable-copy ht #f)]
      [(ht:hashtable mutable?:boolean)
        (cond
          [mutable? (clr-call Hashtable Clone ht)]
          [(clr-is HashtableEx ht)
            (clr-call HashtableEx MakeReadOnly ht)]
          [else
            (clr-new ReadOnlyHashtable ht)])]))
            
  (define/contract (hashtable-keys ht:hashtable)
    (let ((keys (clr-new ArrayList (clr-prop-get Hashtable Keys ht))))
      (clr-call ArrayList ToArray keys)))  
      
  (define/contract (hashtable-mutable? ht:hashtable)
    (not (clr-is ReadOnlyHashtable ht)))
    
  (define (eqv-hash obj)
    (if (null? obj)
        0
        (clr-call Object GetHashCode obj)))
        
  (define/contract (hashtable-equivalence-function ht:hashtable)
    (if (clr-is HashtableEx ht)
        (clr-prop-get HashtableEx EqualityFunction ht)
        eq?))
         
  (define/contract (hashtable-hash-function ht:hashtable)
    (and (clr-is HashtableEx ht)
         (clr-prop-get HashtableEx HashFunction ht)))
  
  (define/contract (hashtable-size ht:hashtable)
    (clr-prop-get Hashtable Count ht))
    
  (define/contract (hashtable-ref ht:hashtable key default)
    (let ((r (clr-indexer-get Hashtable ht key)))
      (if (or (not (null? r)) (hashtable-contains? ht key))
          r
          default)))
      
  (define/contract (hashtable-set! ht:hashtable key obj)
    (clr-indexer-set! Hashtable ht key obj))

  (define/contract (hashtable-delete! ht:hashtable key)
    (clr-call Hashtable Remove ht key))
  
  (define/contract (hashtable-contains? ht:hashtable key)
    (clr-call Hashtable ContainsKey ht key))
    
  (define/contract (hashtable-update! ht:hashtable key proc:procedure default)
    (hashtable-set!
      ht key
      (proc (hashtable-ref ht key default))))

  (define/contract hashtable-clear!
    (case-lambda 
      [(ht)     
        (hashtable-clear! ht 32)]
      [(ht:hashtable k:fixnum)   
        (clr-call Hashtable Clear ht)]))
      
  (define/contract (string-hash str:string)
    (clr-call StringComparer (GetHashCode String)
        (clr-static-prop-get StringComparer Ordinal) 
        (clr-call Object ToString str)))

  (define/contract (string-ci-hash str:string)
    (clr-call StringComparer (GetHashCode String)
        (clr-static-prop-get StringComparer InvariantCultureIgnoreCase) 
        (clr-call Object ToString str)))
        
  (define/contract (symbol-hash sym:symbol)
    (clr-call Object GetHashCode sym))
    
  (define (equal-hash obj)
    (string-hash (format "~a" obj)))    
   

    
)