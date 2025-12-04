#| License
Copyright (c) 2007-2016 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme typed parsing)
  (export
    :
    ->
    parse-lambda-clause
    parse-type
    parse-arg-type
    parse-name-type-expr)
  (import 
    (ironscheme)
    (ironscheme syntax utils)
    (ironscheme typed parsing-helper))

  (define-syntax :
    (lambda (x)
      (define (parse type)
        (syntax-case type (->)
          [(arg ... rest ellipse -> ret)
            (and (identifier? #'rest) (identifier? #'ellipse) (free-identifier=? #'ellipse #'(... ...)))
            (with-syntax (((arg ...) (map parse-type #'(arg ...)))
                          (rest (parse-type #'rest))
                          (ret (parse-type #'ret)))
              #'((arg ...) rest ret))]
          [(arg ... -> ret)
            (with-syntax (((arg ...) (map parse-type #'(arg ...)))
                          (ret (parse-type #'ret)))
              #'((arg ...) ret))]
          [(type arg ...) 
            (with-syntax (((arg ...) (map parse-type #'(arg ...))))
              #'(type arg ...))]
          [type #'type]))
      (syntax-case x ()
        [(_ id type)
          (identifier? #'id)
          (with-syntax [(type-spec-id (syntax-format ":~a" #'id #'id))
                        (type (parse #'type))]
            #'(define-syntax type-spec-id
                (make-compile-time-value 'type)))])))    
                
  (define (parse-lambda-clause x)
    (syntax-case x (->)
      [((arg ... rest ellipse -> ret-type) b b* ...)
        (and (identifier? #'rest) (identifier? #'ellipse) (free-identifier=? #'ellipse #'(... ...)))
        (with-syntax ((((id type) ...) (map parse-arg-type #'(arg ...))))
          (with-syntax (((type ...) (map parse-type #'(type ...)))
                        (ret-type (parse-type #'ret-type)))
            #'((id ... . rest) 
               ((type ...) ret-type)
               b b* ...)))]  
      [((arg ... -> ret-type) b b* ...)
        (with-syntax ((((id type) ...) (map parse-arg-type #'(arg ...))))
          (with-syntax (((type ...) (map parse-type #'(type ...)))
                        (ret-type (parse-type #'ret-type)))
            #'((id ...) 
               ((type ...) ret-type)
               b b* ...)))]        
      [((arg ... . rest) b b* ...)
        (identifier? #'rest)
        (parse-lambda-clause #'((arg ... rest (... ...) -> Object) b b* ...))]
      [((arg ...) b b* ...)
        (parse-lambda-clause #'((arg ... -> Object) b b* ...))]))
     
  (define (parse-arg-type x)
    (syntax-case x (:)
      [(arg : type)
        (identifier? #'arg) 
        #'(arg type)]
      [arg 
        (identifier? #'arg)
        (if (free-identifier=? #'arg #'(... ...))
          (syntax-violation 'parse-arg-type "invalid usage of ...  in macro" #'arg x)
          #'(arg Object))]))
      
  (define (parse-name-type-expr x)
    (syntax-case x (:)
      [(arg expr)
        (with-syntax ((arg (parse-arg-type #'arg)))
          #'(arg expr))])))
    