#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme fsm-cond)
  (export 
    fsm-cond)
  (import
    (ironscheme)
    (ironscheme fsm-cond-helpers))
    
  (define-syntax fsm-cond (fsm-cond-transformer #f)))
          

