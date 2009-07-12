(library (ironscheme symbolic-case)
  (export 
    syntax-match
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
          (identifier? #'=?)
          #'(syntax-case expr ()
              [p 
               (and (=? #'lit 'lit) ... f)
               e] ...)])))
               
  (define-syntax syntax-match
    (lambda (x)
      (define (parse-pattern p)
        (syntax-case p ()
          [(id e ...)
            (if (identifier? #'id)
                #'id
                (parse-pattern #'id))]
          [_ #f]))
      (syntax-case x ()
        [(ctx expr [p e] ...)
          #'(ctx expr [p #t e] ...)]
        [(ctx expr [p f e] ...)
          (with-syntax (((lit ...) 
                         (remq #f (map parse-pattern #'(p ...)))))
            #'(symbolic-case expr (lit ...) [p f e] ...))])))

)