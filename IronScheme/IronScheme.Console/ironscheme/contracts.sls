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
          #'(letrec ((f (lambda/contract (who f) (n ...) body body* ...))) (f v ...))])))
      
       
)   

#|
      
(define/type (a f:boolean . a:integer) 
  (define/type (b x:boolean) x)
  (b f))

(define foo (let/type f ((a:integer 10)(b:list '()))
              (if (zero? a)
                b
                (f (- a 1)(cons a b)))))
                
(printf "~a\n" foo)                
        
(printf "~a\n" ((lambda/type (a:integer-valued b:boolean) (cons a b)) 1 '#f))

(printf "~a\n" ((lambda/type a:integer a) 1 2 3))

(printf "~a\n" (a #f 1 3 4))
|#      
        
          


