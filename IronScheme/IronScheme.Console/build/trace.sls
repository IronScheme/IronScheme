#| License
Copyright (c) 2007-2014 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme trace)
  (export
    make-traced-macro
    make-traced-procedure )
  (import 
    (ironscheme)
    (ironscheme contracts)
    (ironscheme clr)
    (except (ironscheme core) make-traced-macro make-traced-procedure ))
    
  (define/contract make-traced-procedure 
    (case-lambda 
      [(name proc)
        (make-traced-procedure name proc #f)]
      [(name:symbol proc:procedure f)
        (clr-new IronScheme.Runtime.TraceClosure proc name f)]))
    
  (define make-traced-macro 
    (lambda (name x) 
      (cond 
        [(procedure? x)  
         (make-traced-procedure name x syntax->datum)] 
        [(variable-transformer? x) 
         (make-variable-transformer 
           (make-traced-procedure name  
             (variable-transformer-procedure x) 
             syntax->datum))] 
        [else x]))))
