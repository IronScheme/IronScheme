#| License
Copyright (c) 2007-2015 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme typed core)
  (export
    :
    ->
    lambda:
    case-lambda:
    let:
    let*:
    define:
    letrec:
    letrec*:)
  (import 
    (ironscheme)
    (ironscheme typed parsing)
    (ironscheme clr) 
    (ironscheme syntax utils))

  ;; TODO: Add case-lambda version (maybe not?)
  (define-syntax define:
    (lambda (x)
      (define (get-spec id lookup)
        (let ((type-spec (lookup (syntax-format ":~a" id id))))
          (and type-spec
               (datum->syntax id type-spec))))
      (syntax-case x (:)
        [(_ (id arg ...) b b* ...)
          (lambda (lookup)
            (let ((type-spec (get-spec #'id lookup)))
              (if type-spec
                  (with-syntax [(type-spec type-spec)]
                    #'(define id
                        (typed-lambda (arg ...) 
                          type-spec
                          b b* ...)))
                  (with-syntax (((e ...) (parse-lambda-clause #'((arg ...) b b* ...))))
                    #'(define id
                        (typed-lambda e ...))))))]
        [(_ id : type val)
          (with-syntax ((type (parse-type #'type)))
            #'(define id (clr-cast type val)))]
        [(_ id val)
          (lambda (lookup)
            (let ((type-spec (get-spec #'id lookup)))
              (if type-spec            
                  (with-syntax [(type-spec type-spec)]
                    #'(define id (clr-cast type-spec val)))
                  #'(define id val))))])))

  (define-syntax lambda:
    (lambda (x)
      (syntax-case x ()
        [(_ e ...)
          (with-syntax (((e ...) (parse-lambda-clause #'(e ...))))
            #'(typed-lambda e ...))])))
              
  (define-syntax case-lambda:
    (lambda (x)
      (syntax-case x ()
        [(_ e ...)
          (with-syntax (((e ...) (map parse-lambda-clause #'(e ...))))
            #'(typed-case-lambda e ...))])))
            
  (define-syntax let:
    (lambda (x)
      (syntax-case x (->)
        [(_ (arg ... -> ret-type) b b* ...)
          (with-syntax (((((id type) val) ...) (map parse-name-type-expr #'(arg ...))))
            #'((lambda: ((id : type) ... -> ret-type) b b* ...) val ...))]      
        [(_ (arg ...) b b* ...)
          #'(let: (arg ... -> Object) b b* ...)]
        [(_ var (arg ... -> ret-type) b b* ...)
          (with-syntax (((((id type) val) ...) (map parse-name-type-expr #'(arg ...))))
            (with-syntax (((t ...) (generate-temporaries #'(id ...))))
              #'(let: (((t : type) val) ... -> ret-type)
                  (letrec: (((var : (type ... -> ret-type))
                               (lambda: ((id : type) ... -> ret-type)  b b* ...)) -> (type ... -> ret-type))
                    (var t ...)))))]
         [(_ var (arg ...) b b* ...)
          #'(let: var (arg ... -> Object) b b* ...)])))

  (define-syntax let*:
    (syntax-rules (->)
      [(_ (arg1 arg2 ... -> ret-type) b b* ...)
        (let: (arg1 -> ret-type)
          (let*: (arg2 ...  -> ret-type) b b* ...))]
      [(_ (-> ret-type) b b* ...)
        (let: (-> ret-type) b b* ...)]
      [(_ () b b* ...)
        (let: () b b* ...)]
      [(_ (arg1 arg2 ...) b b* ...)
        (let: (arg1)
          (let*: (arg2 ...) b b* ...))]))

  (define-syntax letrec:
    (lambda (x)
      (syntax-case x (->)
        [(_ (arg ... -> ret-type) b1 b2 ...)
          (with-syntax (((((i type) e) ...) (map parse-name-type-expr #'(arg ...))))
           (with-syntax
               (((t ...) (generate-temporaries #'(i ...))))
             #'(let: (((i : type) '()) ... -> ret-type) ; null has special meaning here
                 (let: (((t : type) e) ... -> ret-type)
                   (set! i t) ...
                   (begin b1 b2 ...)))))]
         [(_ (arg ...) b1 b2 ...)
          #'(letrec: (arg ... -> Object) b1 b2 ...)])))
  
  (define-syntax letrec*:
    (lambda (x)
      (syntax-case x (->)
        [(_ (arg ... -> ret-type) b1 b2 ...)
          (with-syntax (((((i type) e) ...) (map parse-name-type-expr #'(arg ...))))
            #'(let: (((i : type) '()) ... -> ret-type) ; null has special meaning here
                (set! i e) ...
                (begin b1 b2 ...)))]
         [(_ (arg ...) b1 b2 ...)
          #'(letrec*: (arg ... -> Object) b1 b2 ...)]))))
