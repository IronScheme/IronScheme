#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme syntax symbolic-case)
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
      (define (parse-clause c)
        (syntax-case c ()
          [(p e)
            #'(p #t e)]
          [(p f e)
            #'(p f e)]))
      (syntax-case x ()
        [(ctx =? expr (lit ...) e ...)
          (and (identifier? #'=?) (for-all identifier? #'(lit ...)))
          (with-syntax [(((p f e) ...) (map parse-clause #'(e ...)))]
            #'(syntax-case expr ()
                [p 
                 (and (=? #'lit 'lit) ... f)
                 e] ...))]))))