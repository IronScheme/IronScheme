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

(library (ironscheme registry)
  (export
    get-registry-value
    set-registry-value!
    )
  (import 
    (rnrs)
    (ironscheme clr))

  (clr-using microsoft.win32)

  (define (get-registry-value key name default)
    (clr-static-call registry getvalue key name default))    

  (define (set-registry-value! key name value)
    (clr-static-call registry setvalue key name value))    
    
 ;; todo: registry keys

)