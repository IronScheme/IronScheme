#| License
Copyright (c) 2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme typed integrable)
  (export
    lambda:
    define-integrable:)
  (import 
    (ironscheme)
    (ironscheme typed))
  
  (define-syntax define-integrable:
    (lambda (x)
      (define make-residual-name
        (lambda (name)
          (datum->syntax name
            (string->symbol
              (string-append "residual-"
                (symbol->string (syntax->datum name)))))))
      (syntax-case x (lambda:)
        [(_ (name arg ...) form1 form2 ...)
          #'(define-integrable: name (lambda: (arg ...) form1 form2 ...))]
        [(_ name (lambda: (a ...) form1 form2 ...))
         (identifier? #'name)
         (with-syntax ((xname (make-residual-name #'name)))
           #'(begin
               (define-fluid-syntax name
                 (lambda (x)
                   (syntax-case x ()
                     [_ (identifier? x) #'xname]
                     [(_ arg (... ...))
                      #'((fluid-let-syntax
                           ((name (identifier-syntax xname)))
                           (lambda: (a ...) form1 form2 ...))
                         arg (... ...))])))
               (define xname
                 (fluid-let-syntax ((name (identifier-syntax xname)))
                   (lambda: (a ...) form1 form2 ...)))))]))))