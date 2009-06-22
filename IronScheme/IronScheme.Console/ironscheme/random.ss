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
    (ironscheme clr))

  (clr-using system)
  
  (define make-random
    (case-lambda
      [()       (clr-new random)]
      [(seed)   (clr-new random seed)]))
      
  (define next-fixnum
    (case-lambda
      [(rg)         (clr-call random next rg)]
      [(rg max)     (clr-call random next rg max)]
      [(rg min max) (clr-call random next rg min max)]))

  (define (next-bytevector rg bytevector)
    (clr-call random nextbytes rg bytevector))
          
  (define (next-flonum rg)
    (clr-call random nextdouble rg))

)