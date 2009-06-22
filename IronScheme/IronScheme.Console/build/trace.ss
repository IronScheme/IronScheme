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

(library (ironscheme trace)
  (export
    make-traced-macro)
  (import 
    (ironscheme)
    (except (ironscheme core) make-traced-macro))
    
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
        [else x])))
        
)        
        
          


