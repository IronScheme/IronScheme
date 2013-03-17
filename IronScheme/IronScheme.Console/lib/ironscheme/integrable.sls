#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme integrable)
  (export
    lambda
    typed-lambda
    define-integrable
    define-typed-integrable)
  (import (ironscheme))
  
  (define-syntax define-integrable
    (lambda (x)
      (define make-residual-name
        (lambda (name)
          (datum->syntax name
            (string->symbol
              (string-append "residual-"
                (symbol->string (syntax->datum name)))))))
      (syntax-case x (lambda)
        [(_ (name . formals) form1 form2 ...)
          #'(define-integrable name (lambda formals form1 form2 ...))]
        [(_ name (lambda formals form1 form2 ...))
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
                           (lambda formals form1 form2 ...))
                         arg (... ...))])))
               (define xname
                 (fluid-let-syntax ((name (identifier-syntax xname)))
                   (lambda formals form1 form2 ...)))))])))  
                   
  (define-syntax define-typed-integrable
    (lambda (x)
      (define make-residual-name
        (lambda (name)
          (datum->syntax name
            (string->symbol
              (string-append "residual-"
                (symbol->string (syntax->datum name)))))))
      (syntax-case x (typed-lambda)
        [(_ (name . formals) type-spec form1 form2 ...)
          #'(define-typed-integrable name (typed-lambda formals type-spec form1 form2 ...))]
        [(_ name (typed-lambda formals type-spec form1 form2 ...))
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
                           (typed-lambda formals type-spec form1 form2 ...))
                         arg (... ...))])))
               (define xname
                 (fluid-let-syntax ((name (identifier-syntax xname)))
                   (typed-lambda formals type-spec form1 form2 ...)))))]))))