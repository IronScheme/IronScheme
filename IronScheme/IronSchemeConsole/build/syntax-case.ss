(library (ironscheme syntax-case)
  (export
    make-variable-transformer
    
    syntax-case
    syntax
    
    identifier?
    bound-identifier=?
    free-identifier=?
    
    syntax->datum
    datum->syntax
    
    generate-temporaries
    
    with-syntax
    
    quasisyntax
    unsyntax
    unsyntax-splicing
    
    syntax-violation
    
    define-syntax-case)
    
  (import (rnrs))
  
  ;;Michele Simionato
  (define-syntax define-syntax-case
    (syntax-rules ()
      ((_ name (literal ...)
          ((ctx arg ...) templ) ...)
       (define-syntax name
         (lambda (x)
           (syntax-case x (<expand> literal ...)
             ((ctx <expand> arg ...) #`'templ) ...
             ((ctx arg ...) #`templ) ...))))
      ((_ name (literal ...)
          ((ctx arg ...) fender templ) ...)
       (define-syntax name
         (lambda (x)
           (syntax-case x (<expand> literal ...)
             ((ctx <expand> arg ...) fender #`'templ) ...
             ((ctx arg ...) fender #`templ) ...))))
      ))

)