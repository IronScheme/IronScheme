#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme syntax define-macro)
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
                     (datum->syntax #'k (apply expander lst)))))))]))))
