#| License
Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

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
    