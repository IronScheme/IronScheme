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

(library (ironscheme mutable-pairs)
  (export
    set-car!
    set-cdr!)
    
  (import 
    (rnrs)
    (ironscheme clr))
    
  (define (set-car! lst val)
    (unless (pair? lst)
      (assertion-violation 'set-car! "not a pair" val))
    (clr-field-set! IronScheme.Runtime.Cons car lst val))      
    
  (define (set-cdr! lst val)
    (unless (pair? lst)
      (assertion-violation 'set-cdr! "not a pair" val))
    (clr-field-set! IronScheme.Runtime.Cons cdr lst val))      
  
)

