(library (ironscheme symbolic-case)
  (export 
    syntax-casep 
    symbolic=? 
    symbolic-case)
  (import (ironscheme))

  (define (symbolic=? x y)
    (eq? (syntax->datum x) y))
    
  (define-syntax symbolic-case
    (lambda (x)
      (syntax-case x ()
        [(_ e ...)
          #'(syntax-casep symbolic=? e ...)])))
    
  (define-syntax syntax-casep
    (lambda (x)
      (syntax-case x ()
        [(ctx =? expr (lit ...) [p e] ...)
          #'(ctx =? expr (lit ...) [p #t e] ...)]
        [(ctx =? expr (lit ...) [p f e] ...)
          (and (identifier? #'=?) (for-all identifier? #'(lit ...)))
          #'(syntax-case expr ()
              [p 
               (and (=? #'lit 'lit) ... f)
               e] ...)])))

)