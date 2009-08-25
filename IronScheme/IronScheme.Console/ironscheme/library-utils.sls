#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

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
    