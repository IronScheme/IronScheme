#| License
Copyright (c) 2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme typed)
  (export
    :
    ->
    lambda:
    let:
    let*:
    define:
    letrec:
    letrec*:)
  (import 
    (ironscheme)
    (ironscheme clr) 
    (ironscheme syntax-format))
    
  (define-syntax ->
    (lambda (x)
      (syntax-violation '-> "invalid usage of auxilliary keyword" x)))    

  (define-syntax :
    (lambda (x)
      (define (parse-type type)
        (syntax-case type (->)
          [(arg ... -> ret)
            #'((arg ...) ret)]
          [type #'type]))  
      (syntax-case x ()
        [(_ id type)
          (identifier? #'id)
          (with-syntax [(type-spec-id (syntax-format ":~a" #'id #'id))
                        (type (parse-type #'type))]
            #'(define-syntax type-spec-id
                (make-compile-time-value 'type)))])))
                
  (define-syntax define:
    (lambda (x)
      (define (get-spec id lookup)
        (let ((type-spec (lookup (syntax-format ":~a" id id))))
          (unless type-spec
            (syntax-violation (syntax->datum #'id) "type spec not found" x))
          (datum->syntax id type-spec)))
      (syntax-case x ()              
        [(_ (id args ...) body)
          (lambda (lookup)
            (with-syntax [(type-spec (get-spec #'id lookup))]
              #'(define id
                  (typed-lambda (args ...) 
                    type-spec
                    body))))]
        [(_ id val)
          (lambda (lookup)
            (with-syntax [(type-spec (get-spec #'id lookup))]
              #'(define id (clr-cast type-spec val))))])))                

  (define-syntax lambda:
    (syntax-rules (:)
      [(_ ((id : type) ...) : ret-type b b* ...)
        (typed-lambda (id ...) 
                      ((type ...) ret-type)
                      b b* ...)]
      [(_ ((id : type) ...) b b* ...)
        (lambda: ((id : type) ...) : Object b b* ...)]))

  (define-syntax let:
    (syntax-rules (:)
      [(_ ((id : type val) ...) : ret-type b b* ...)
        ((lambda: ((id : type) ...) : ret-type b b* ...) val ...)]
      [(_ ((id : type val) ...) b b* ...)
        (let: ((id : type val) ...) : Object b b* ...)]      
      [(_ var ((id : type val) ...) : ret-type b b* ...)  
        (letrec: ((var : (IronScheme.Runtime.Typed.TypedClosure type ... ret-type) 
                    (lambda: ((id : type) ...) : ret-type b b* ...))) : ret-type
          (var val ...))]
      [(_ var ((id : type val) ...) b b* ...)
        (let: var ((id : type val) ...) : Object b b* ...)]))
        
  (define-syntax let*:
    (syntax-rules (:)
      [(_ () : ret-type b b* ...)
        (let: () : ret-type b b* ...)]
      [(_ () b b* ...)
        (let: () : Ojbect b b* ...)]
      [(_ ((name1 : type1 expr1) (name2 : type2 expr2) ...) : ret-type b b* ...)
        (let: ((name1 : type1 expr1))
          (let*: ((name2 : type2 expr2) ...) : ret-type
            b b* ...))]
      [(_ ((name1 : type1 expr1) (name2 : type2 expr2) ...) b b* ...)
        (let*: ((name1 : type1 expr1) (name2 : type2 expr2) ...) : Object b b* ...)]))
                      
  (define-syntax letrec:
    (lambda (x)
      (syntax-case x (:)
        [(_ ((i : type e) ...) : ret-type b1 b2 ...)
         (with-syntax
             (((t ...) (generate-temporaries #'(i ...))))
           #'(let: ((i : type '()) ...) : ret-type
               (let: ((t : type e) ...) : ret-type
                 (set! i t) ...
                 (begin b1 b2 ...))))]
        [(_ ((i : type e) ...) b1 b2 ...)
          #'(letrec: ((i : type e) ...) : Object b1 b2 ...)])))
                 
  (define-syntax letrec*:
    (syntax-rules (:)
        [(_ ((i : type e) ...) : ret-type b1 b2 ...)
          (let: ((t : type '()) ...) : ret-type
            (set! t e) ...
            (begin b1 b2 ...))]
        [(_ ((i : type e) ...) b1 b2 ...)
          (letrec*: ((i : type e) ...) : Object b1 b2 ...)])))