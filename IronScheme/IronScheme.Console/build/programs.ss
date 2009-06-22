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

(library (ironscheme programs)
  (export
    exit)
    
  (import 
    (except (rnrs) exit)
    (ironscheme clr))
  
  (define exit
    (case-lambda
      [() (exit 0)]
      [(reason)
        (let ((r (or (and (not reason) 1) reason)))
          (unless (fixnum? r)
            (assertion-violation 'exit "not an integer" r))
          (clr-static-call Environment Exit r))]))
        
)