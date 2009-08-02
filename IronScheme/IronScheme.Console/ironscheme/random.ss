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

(library (ironscheme random)
  (export
    make-random
    next-fixnum
    next-bytevector
    next-flonum 
    )
  (import 
    (rnrs)
    (ironscheme contracts)
    (ironscheme clr))

  (clr-using system)
  
  (define (rng? obj)
    (clr-is Random obj))
  
  (define make-random
    (case/contract
      [()             (clr-new Random)]
      [(seed:fixnum)  (clr-new Random seed)]))
      
  (define next-fixnum
    (case/contract
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

)