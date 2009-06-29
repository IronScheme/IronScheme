(import (rnrs))


(define-syntax symbolic-case
  (lambda (x)
    (define (make-fenders ctx lits patterns fenders)
      (map (lambda (p f) 
             (map (lambda (lit)
                    ())
             '?) 
           patterns 
           fenders))
    (syntax-case x ()
      [(ctx expr (lit ...) [p e] ...)
        #'(ctx expr (lit ...) [p #t e] ...)]
      [(ctx expr (lit ...) [p f e] ...)
        (with-syntax (((f ...) (make-fenders 
                                #'ctx
                                (syntax->datum #'(lit ...))
                                #'(p ...)
                                #'(f ...))))
          #'(syntax-case expr ()
              [p f e] ...))])))

