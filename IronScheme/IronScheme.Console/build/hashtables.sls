#| License
Copyright (c) 2007-2016 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

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
    (except (ironscheme)
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
    (except (ironscheme core) eqv-hash)
    (ironscheme contracts)
    (ironscheme clr)
    (ironscheme typed)
    (ironscheme unsafe))
    
  (clr-using System.Collections)
  (clr-using IronScheme.Runtime.R6RS)
  
  (define: (hashtable? obj -> bool)
    (clr-is Hashtable obj))
    
  (define-syntax to-null
    (syntax-rules ()
      [(_ o)
        (clr-static-call Hashtables ToNull o)]))

  (define-syntax from-null
    (syntax-rules ()
      [(_ o)
        (clr-static-call Hashtables FromNull o)]))
  
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
      (vector-map (lambda (k) (from-null k)) (clr-call ArrayList ToArray keys))))  
      
  (define/contract (hashtable-mutable? ht:hashtable)
    (not (clr-is ReadOnlyHashtable ht)))
    
  (define (eqv-hash obj)
    (if (null? obj)
        0
        (clr-call Object GetHashCode obj)))
        
  (define/contract (hashtable-equivalence-function ht:hashtable)
    (cond 
      [(clr-is HashtableEx ht)
        (clr-prop-get HashtableEx EqualityFunction ht)]
      [(clr-is ReadOnlyHashtable ht)
        (let ((ef (clr-prop-get ReadOnlyHashtable EqualityFunction ht)))
          (if (null? ef)
              eq?
              ef))]
      [else  eq?]))
         
  (define/contract (hashtable-hash-function ht:hashtable)
    (cond 
      [(clr-is HashtableEx ht)
        (clr-prop-get HashtableEx HashFunction ht)]
      [(clr-is ReadOnlyHashtable ht)
        (let ((hf (clr-prop-get ReadOnlyHashtable HashFunction ht)))
          (if (null? hf)
              #f
              hf))]
      [else  #f]))
  
  (define/contract (hashtable-size ht:hashtable)
    (clr-prop-get Hashtable Count ht))
    
  (define/contract (hashtable-ref ht:hashtable key default)
    (let ((key (to-null key)))
      (let ((r (clr-indexer-get Hashtable ht key)))
        (if (or (not (null? r)) (hashtable-contains? ht key))
            r
            default))))
      
  (define/contract (hashtable-set! ht:hashtable key obj)
    (clr-indexer-set! Hashtable ht (to-null key) obj))

  (define/contract (hashtable-delete! ht:hashtable key)
    (clr-call Hashtable Remove ht (to-null key)))
  
  (define/contract (hashtable-contains? ht:hashtable key)
    (clr-call Hashtable ContainsKey ht (to-null key)))
    
  (define/contract (hashtable-update! ht:hashtable key proc:procedure default)
    (let ((key (to-null key)))
      (hashtable-set!
        ht key
        (proc (hashtable-ref ht key default)))))

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
    (define-syntax clobber
      (syntax-rules ()
        [(_ a b)
          ($fx+ ($fx* a 33) b)]))
    (define ht (make-eq-hashtable))
    ($fxand 
      #x7FFFFFFF
      (let hash ((obj obj))
        (cond
          [(or (null? obj) (hashtable-contains? ht obj)) 0]
          [else 
            (hashtable-set! ht obj obj)
            (cond
              [(string? obj)
                (string-hash obj)]
              [(pair? obj)
                (clobber 
                  (hash (car obj))
                  (hash (cdr obj)))]
              [(vector? obj)
                (vector-fold-left 
                  (lambda (a v)
                    (clobber a (hash v)))
                  100 obj)]
              [(bytevector? obj)
                (bytevector-fold-left 
                  (lambda (a v)
                    (clobber a (eqv-hash v)))
                  200 obj)]
              [else 
                (eqv-hash obj)])])))))