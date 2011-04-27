#| License
Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

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