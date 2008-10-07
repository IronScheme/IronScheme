(library (list-match)
  (export list-match)
  (import (rnrs))

  (define-syntax list-match
    (syntax-rules ()
      ((_ expr (pattern fender ... template) ...)
        (let ((obj expr))
          (cond ((list-match-aux obj pattern fender ...
                  (list template)) => car) ...
                 (else (error 'list-match "pattern failure")))))))
  
  (define-syntax list-match-aux
    (lambda (stx)
      (define (underscore? x)
        (and (identifier? x) (free-identifier=? x (syntax _))))
      (syntax-case stx (quote quasiquote)
        ((_ obj pattern template)
          (syntax (list-match-aux obj pattern #t template)))
        ((_ obj () fender template)
          (syntax (and (null? obj) fender template)))
        ((_ obj underscore fender template)
          (underscore? (syntax underscore))
          (syntax (and fender template)))
        ((_ obj var fender template)
          (identifier? (syntax var))
          (syntax (let ((var obj)) (and fender template))))
        ((_ obj (quote datum) fender template)
          (syntax (and (equal? obj (quote datum)) fender template)))
        ((_ obj (quasiquote datum) fender template)
          (syntax (and (equal? obj (quasiquote datum)) fender template)))
        ((_ obj (kar . kdr) fender template)
          (syntax (and (pair? obj)
                  (let ((kar-obj (car obj)) (kdr-obj (cdr obj)))
                    (list-match-aux kar-obj kar
                          (list-match-aux kdr-obj kdr fender template))))))
        ((_ obj const fender template)
          (syntax (and (equal? obj const) fender template))))))          
  
  
)
