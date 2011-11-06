(import (ironscheme) (ironscheme syntax-format) (ironscheme unsafe))

(define-syntax :
  (lambda (x)
    (syntax-case x ()
      [(_ id type)
        (identifier? #'id)
        (with-syntax [(type-spec-id (syntax-format ":~a" #'id #'id))]
          #'(define-syntax type-spec-id
              (make-compile-time-value 'type)))])))


(define-syntax lambda:
  (lambda (x)
    (syntax-case x (:)
      [(_ ((id : type) ...) : ret-type b b* ...)
        #'(typed-lambda (id ...) 
                        ((type ...) ret-type)
                        b b* ...)])))

(define-syntax let:
  (syntax-rules (:)
    [(_ ((id : type val) ...) : ret-type b b* ...)
      ((lambda: ((id : type) ...) : ret-type b b* ...) val ...)]
    [(_ var ((id : type val) ...) : ret-type b b* ...)  
      (letrec: ((var : (IronScheme.Runtime.Typed.TypedClosure type ... ret-type) 
                  (lambda: ((id : type) ...) : ret-type b b* ...))) : ret-type
        (var val ...))]))
              
(define-syntax define:
  (lambda (x)
    (syntax-case x ()              
      [(_ (id args ...) body)
        (with-syntax [(type-spec-id (syntax-format ":~a" #'id #'id))]
          (lambda (lookup)
            (let ((type-spec (lookup #'type-spec-id)))
              (unless type-spec
                (syntax-violation (syntax->datum #'id) "Type spec not found" x))
              (with-syntax [(type-spec (datum->syntax #'id type-spec))]
                #'(define id
                    (typed-lambda (args ...) 
                      type-spec
                      body))))))])))
                    
(define-syntax letrec:
  (lambda (x)
    (syntax-case x (:)
      ((_ ((i : type e) ...) : ret-type b1 b2 ...)
       (with-syntax
           (((t ...) (generate-temporaries #'(i ...))))
         #'(let: ((i : type '()) ...) : ret-type
             (let: ((t : type e) ...) : ret-type
               (set! i t) ...
               (begin b1 b2 ...))))))))