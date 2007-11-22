(library (rnrs hashtables (6))
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
  (import (rnrs))
  
   
)