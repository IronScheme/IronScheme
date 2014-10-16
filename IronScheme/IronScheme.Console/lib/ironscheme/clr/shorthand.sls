#| License
Copyright (c) 2007-2014 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme clr shorthand)
  (export
    define-clr-type
    lambda-clr-type
    let-clr-type
    with-clr-type
    ;;convenience
    clr-using)
  (import 
    (ironscheme)
    (ironscheme typed core)
    (ironscheme clr shorthand-helper)
    (ironscheme clr))
  
  (define-syntax define-clr-type
    (lambda (x)
      (syntax-case x ()
        [(_ (name (id type) ...) b b* ...)
          (for-all identifier? #'(id ... type ...))
          #'(define name 
              (typed-lambda (id ...) ((type ...) Object) 
                (with-clr-type ((id type) ...)
                  b b* ...)))]
        [(_ name type val)
          #'(begin
              (define: _name : type val)
              (define-syntax name
                (lambda (x)
                  (syntax-case x ()
                    [(_ arg (... ...))
                      #'(with-clr-type ((_name type))
                          (_name arg (... ...)))]
                    [_ #'_name]))))])))  
            
  (define-syntax lambda-clr-type
    (lambda (x)
      (syntax-case x ()
        [(_ ((id type) ...) b b* ...)
          (for-all identifier? #'(id ... type ...))
          #'(typed-lambda (id ...) ((type ...) Object) 
              (with-clr-type ((id type) ...)
                b b* ...))])))

  (define-syntax let-clr-type 
    (lambda (x)
      (syntax-case x ()
        [(_ ((id (type arg ...)) ...) b b* ...)
          (for-all identifier? #'(id ... type ...))
          #'((typed-lambda (id ...) ((type ...) Object)
               (with-clr-type ((id type) ...)
                  b b* ...))
              (clr-new type arg ...) ...)])))

  (define-syntax with-clr-type
    (lambda (x)
      (syntax-case x ()
        [(_ ((id type) ...) b b* ...)
          (for-all identifier? #'(id ... type ...))
          (with-syntax (((e ...) (map parse-clause #'(id ...) #'(type ...))))
            #'(let-syntax ((id e) ...) b b* ...))]))))