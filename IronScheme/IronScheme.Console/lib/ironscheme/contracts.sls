#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme contracts)
  (export
    lambda
    case-lambda
    case/contract
    lambda/contract
    define/contract
    let/contract)
  (import 
    (ironscheme) 
    (ironscheme contracts-helper))
  
  ; not exported  
  (define-syntax who 
    (lambda (x) 
      (syntax-violation #f "invalid use of auxiliary keyword" x)))
    
  (define-syntax lambda/contract  
    (lambda (x)
      (syntax-case x (who)
        [( _ (who l) e1 e2 e* ...)
          (with-syntax (((e ...) (parse-body #'l #'(e1 e2 e* ...))))
            #'(lambda e ...))]
        [(_ e1 e2 e* ...)
          #'(lambda/contract (who #f) e1 e2 e* ...)])))

  (define-syntax case/contract  
    (lambda (x)
      (syntax-case x (who)
        [(_ (who l) e ...)
          (with-syntax (((e ...) (map (lambda (x) (parse-body #'l x)) #'(e ...))))
            #'(case-lambda e ...))]
        [(_ e ...)
          #'(case/contract (who #f) e ...)])))
                  
  (define-syntax define/contract
    (lambda (x)
      (syntax-case x (case-lambda lambda)
        [(_ name (lambda e ...))
          (identifier? #'name)
          #'(define name 
              (lambda/contract (who name) e ...))]
        [(_ name (case-lambda e ...))
          (identifier? #'name)
          #'(define name 
              (case/contract (who name) e ...))]
        [(_ (name . formals) body body* ...)
          (identifier? #'name)
          #'(define name 
              (lambda/contract (who name) formals body body* ...))])))
          
  (define-syntax let/contract
    (lambda (x)
      (syntax-case x ()
        [(_ ((n v) ...) body body* ...)
          #'((lambda/contract (n ...) body body* ...) v ...)]
        [(_ f ((n v) ...) body body* ...)
          (identifier? #'f)
          #'(letrec ((f (lambda/contract (who f) (n ...) body body* ...))) (f v ...))]))))   