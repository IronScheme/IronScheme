(import (ironscheme) (ironscheme syntax-format))

(define-syntax :
  (lambda (x)
    (syntax-case x ()
      [(_ id type)
        (identifier? #'id)
        (with-syntax [(type-spec-id (syntax-format ":~a" #'id #'id))]
          #'(define-syntax type-spec-id
              (make-compile-time-value 'type)))])))
              
(define-syntax define:
  (lambda (x)
    (syntax-case x ()              
      [(_ (id args ...) body)
        (with-syntax [(type-spec-id (syntax-format ":~a" #'id #'id))]
          (lambda (lookup)
            (with-syntax [(type-spec (datum->syntax #'id (lookup #'type-spec-id)))]
              #'(define id
                  (typed-lambda (args ...) 
                    type-spec
                    body)))))])))