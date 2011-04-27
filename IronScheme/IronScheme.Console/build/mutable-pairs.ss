#| License
Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme mutable-pairs)
  (export
    set-car!
    set-cdr!)
  (import 
    (rnrs)
    (ironscheme contracts)
    (ironscheme clr))

  (clr-using IronScheme.Runtime)   
    
  (define/contract (set-car! lst:pair val)
    (clr-field-set! Cons car lst val))      
    
  (define/contract (set-cdr! lst:pair val)
    (clr-field-set! Cons cdr lst val))      
  
)

