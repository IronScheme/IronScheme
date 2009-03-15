(library (srfi land)
  (export land*)
  (import (rnrs))
	
  (define-syntax land*
    (lambda (x)
      (define free-id (car (generate-temporaries '(_))))
      (define (bound-identifier? x)
        (and (identifier? x)
             (not
               (free-identifier=? x
                 (datum->syntax free-id (syntax->datum x))))))
      (syntax-case x ()
        [(_ ()) #t]
        [(_ (claws ...) b b* ...) 
         #'(land* (claws ... (tmp (begin b b* ...))))]
        [(_ ([var expr])) #'expr]
        [(_ ([expr]))     #'expr]
        [(_ (var)) 
         (if (bound-identifier? #'var) 
             #'var
             (error #'var "var is unbound in land* clause"))]
        [(_ ([var expr] claws ...)) (identifier? #'var)
         #'(cond
             [expr => (lambda (var) (land* (claws ...)))]
             [else #f])]
        [(_ ([expr] claws ...))
         #'(and expr (land* (claws ...)))]
        [(_ (var claws ...)) 
         (if (bound-identifier? #'var)
             #'(and var (land* (claws ...)))
             (error #'var "var is unbound in land* clause"))])))
)