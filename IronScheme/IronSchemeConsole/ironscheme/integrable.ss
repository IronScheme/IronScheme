(library (ironscheme integrable)
  (export
    lambda
    define-integrable)
  (import (ironscheme))
  
  (define-syntax define-integrable
    (lambda (x)
      (define make-residual-name
        (lambda (name)
          (datum->syntax name
            (string->symbol
              (string-append "residual-"
                (symbol->string (syntax->datum name)))))))
      (syntax-case x (lambda)
        [(_ (name . formals) form1 form2 ...)
          (identifier? #'name)
          #'(define-integrable name (lambda formals form1 form2 ...))]
        [(_ name (lambda formals form1 form2 ...))
         (identifier? #'name)
         (with-syntax ((xname (make-residual-name #'name)))
           #'(begin
               (define-syntax name
                 (lambda (x)
                   (syntax-case x ()
                     [_ (identifier? x) #'xname]
                     [(_ arg (... ...))
                      #'((fluid-let-syntax
                           ((name (identifier-syntax xname)))
                           (lambda formals form1 form2 ...))
                         arg (... ...))])))
               (define xname
                 (fluid-let-syntax ((name (identifier-syntax xname)))
                   (lambda formals form1 form2 ...)))))])))  

)