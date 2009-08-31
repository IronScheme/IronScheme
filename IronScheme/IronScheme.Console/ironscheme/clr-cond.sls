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
    (ironscheme strings)
    (ironscheme clr)
    (ironscheme unsafe)
    (ironscheme clr reflection)
    (ironscheme fsm-cond-helpers))

  (define-syntax clr-cond
    (lambda (x)
      (define (gen-predicate pred id)
        (list pred
              id
              (let ((x (symbol->string (syntax->datum pred))))
                (if (string-ends-with? x "?")
                    (let ((p (string->symbol (substring x 0 (- (string-length x) 1)))))
                      #`($or (null? #,id) (clr-is #,(datum->syntax pred p) #,id)))
                    #`(clr-is #,pred #,id)))))
      (syntax-case x ()
        [(_ (id ...) ((pred ...) expr) ...)
          (let ((preds (vector->list (get-predicates #'(pred ... ...)))))
            (with-syntax ((((pred* x test) ...) (map gen-predicate 
                                                     preds 
                                                     (generate-temporaries preds))))
              #'(let-syntax
                    ((pred* (syntax-rules () [(_ x) test])) ...)
                  (clr-cond-aux (id ...)
                                ((pred ...) expr) ...))))])))

  (define-syntax clr-cond-aux
    (generator #f))

  (define-syntax clr-method-ref
    (lambda (x)
      (syntax-case x ()
        [(_ type method (arg-type ...) ...)
          #'(case-lambda )]))))
    

          
          

