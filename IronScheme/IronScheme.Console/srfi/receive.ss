(library (srfi receive)
  (export receive)
  (import (rnrs))

  (define-syntax receive
    (syntax-rules ()
      ((receive formals expression body body* ...)
       (call-with-values (lambda () expression)
         (lambda formals body body* ...)))))

)
