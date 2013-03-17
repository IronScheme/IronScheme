#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme registry)
  (export
    get-registry-value
    set-registry-value!)
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