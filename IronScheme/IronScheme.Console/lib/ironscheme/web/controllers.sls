#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme web controllers)
  (export
    get
    post
    define-action)
  (import
    (ironscheme)
    (ironscheme web))
    
  (define-syntax define-aux
    (syntax-rules ()
      [(_ id ...)
        (begin
          (define-syntax id
            (lambda (x)
              (syntax-violation #f "invalid use of auxiliary keyword" x 'id))) ...)]))
            
  (define-aux get post)  
  
  (define (get-value/default key)
    (or (get-value key)
        (querystring '()))) ; for default use null

  (define (get-value key)
    (if (symbol? key)
        (get-value (symbol->string key))
        (or (context-item key) (form key) (querystring key))))
    
  (define-syntax define-action
    (lambda (x)
      (syntax-case x ()
        [(_ (name first rest ...) body body* ...)
          #'(define (name)
              ((lambda (first rest ...)
                body body* ...) 
                (get-value/default 'first) (get-value 'rest) ...))]        
        [(_ (name arg ...) body body* ...)
          #'(define (name)
              ((lambda (arg ...)
                body body* ...) 
                (get-value 'arg) ...))]
        [(_ name [(action first rest ...) body body* ... ] ...)
          (for-all 
            (lambda (i)
              (or
                (free-identifier=? i #'get)
                (free-identifier=? i #'post)))
            #'(action ...))
          #'(define (name)
              (case (http-method)
                [(action) 
                  ((lambda (first rest ...)
                      body body* ...) 
                    (get-value/default 'first) (get-value 'rest) ...)] ...))]        
        [(_ name [(action arg ...) body body* ... ] ...)
          (for-all 
            (lambda (i)
              (or
                (free-identifier=? i #'get)
                (free-identifier=? i #'post)))
            #'(action ...))
          #'(define (name)
              (case (http-method)
                [(action) 
                  ((lambda (arg ...)
                      body body* ...) 
                    (get-value 'arg) ...)] ...))]))))    