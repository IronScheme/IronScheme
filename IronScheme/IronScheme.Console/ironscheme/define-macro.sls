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

(library (ironscheme define-macro)
  (export define-macro)
  (import (rnrs))

  (define-syntax define-macro
     (lambda (x)
       (syntax-case x ()
         [(_ (name . params) body1 body2 ...)
          #'(define-macro name (lambda params body1 body2 ...))]
         [(_ name expander)
          #'(define-syntax name
              (lambda (y)
                (syntax-case y ()
                  ((k . args)
                   (let ((lst (syntax->datum #'args)))
                     (datum->syntax #'k (apply expander lst)))))))]
          )))
)
