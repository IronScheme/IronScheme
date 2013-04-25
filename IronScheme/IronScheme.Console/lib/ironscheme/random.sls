#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme random)
  (export
    make-random
    next-fixnum
    next-bytevector
    next-flonum 
    random)
  (import 
    (rnrs)
    (ironscheme contracts)
    (ironscheme clr))
  
  (define (rng? obj)
    (clr-is Random obj))
  
  (define/contract make-random
    (case-lambda
      [()             (clr-new Random)]
      [(seed:fixnum)  (clr-new Random seed)]))
      
  (define/contract next-fixnum
    (case-lambda
      [(rg:rng)         
        (clr-call Random Next rg)]
      [(rg:rng max:fixnum)     
        (clr-call Random Next rg max)]
      [(rg:rng min:fixnum max:fixnum) 
        (clr-call Random Next rg min max)]))

  (define/contract (next-bytevector rg:rng bytevector:bytevector)
    (clr-call Random NextBytes rg bytevector))
          
  (define/contract (next-flonum rg:rng)
    (clr-call Random NextDouble rg))
  
  ; make parameter like in Racket?
  (define rng (make-random))
    
  (define/contract random 
    (case-lambda 
      [() (next-flonum rng)]
      [(n:fixnum) (next-fixnum rng n)])))