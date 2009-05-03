(library (ironscheme library-utils)
  (export
    import-and-reexport-all-from)
  (import (ironscheme))

  (define-syntax import-and-reexport-all-from 
    (lambda (x) 
      (syntax-case x () 
        [(ctxt impspec* ...) 
         (cons #'begin 
           (map 
             (lambda (lib) 
               (let ([sym* (environment-symbols 
                             (environment (syntax->datum lib)))]) 
                 (with-syntax ([lib lib] 
                               [(id* ...) (datum->syntax #'ctxt sym*)] 
                               [(t* ...) (generate-temporaries sym*)]) 
                    #'(begin 
                        (import (rename lib (id* t*) ...)) 
                        (export (rename (t* id*) ...)))))) 
             #'(impspec* ...)))]))))
    