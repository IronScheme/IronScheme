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
    (ironscheme contracts)
    (ironscheme clr))

  (clr-using Microsoft.Win32)

  (define/contract (get-registry-value key:string name:string default)
    (clr-static-call Registry GetValue key name default))    

  (define/contract (set-registry-value! key:string name:string value)
    (clr-static-call Registry SetValue key name value))    
    
 ;; todo: registry keys

)