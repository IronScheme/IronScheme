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

(library (ironscheme clr-cond)
  (export 
    clr-cond)
  (import
    (ironscheme)
    (ironscheme clr)
    (ironscheme fsm-cond-helpers))

  (define-syntax clr-cond
    (lambda (x)
      (syntax-case x ()
        [(_ (id ...) ((pred ...) expr) ...)
          (with-syntax ((#(pred* ...) (get-predicates #'(pred ... ...))))
            #'(let-syntax 
                  ((pred* (syntax-rules () 
                            [(_ x) 
                              (clr-is pred* x)])) ...)
                (clr-cond-aux (id ...)
                              ((pred ...) expr) ...)))])))
    
  (define-syntax clr-cond-aux
    (generator #f)))

          
          

