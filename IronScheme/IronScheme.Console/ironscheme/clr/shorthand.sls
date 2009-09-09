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

(library (ironscheme clr shorthand)
  (export
    ;define-global-clr-type 
    define-clr-type
    lambda-clr-type
    let-clr-type
    with-clr-type
    ;;convenience
    clr-using)
  (import 
    (ironscheme)
    (ironscheme clr shorthand-helper)
    (ironscheme clr))
  
  (define-syntax define-clr-type
    (lambda (x)
      (syntax-case x ()
        [(_ (name (id type) ...) b b* ...)
          (for-all identifier? #'(id ... type ...))
          #'(define (name id ...)
              (with-clr-type ((id type) ...)
                b b* ...))])))  
            
  (define-syntax lambda-clr-type
    (lambda (x)
      (syntax-case x ()
        [(_ ((id type) ...) b b* ...)
          (for-all identifier? #'(id ... type ...))
          #'(lambda (id ...)
              (with-clr-type ((id type) ...)
                b b* ...))])))

  (define-syntax let-clr-type 
    (lambda (x)
      (syntax-case x ()
        [(_ ((id (type arg ...)) ...) b b* ...)
          (for-all identifier? #'(id ... type ...))
          #'(let ((id (clr-new type arg ...)) ...)
              (with-clr-type ((id type) ...)
                b b* ...))])))         

  (define-syntax with-clr-type
    (lambda (x)
      (syntax-case x ()
        [(_ ((id type) ...) b b* ...)
          (for-all identifier? #'(id ... type ...))
          (with-syntax (((e ...) (map parse-clause #'(id ...) #'(type ...))))
            #'(let-syntax ((id e) ...) b b* ...))])))  
  )