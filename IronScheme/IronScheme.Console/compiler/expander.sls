(library (compiler expander)
  (export 
    sexp/expand
    sexp/expand*
    make-sexp-environment
    )
  (import 
    (except (ironscheme) define-record-type syntax->datum datum->syntax error)
    (only (srfi :1) last append-reverse reduce)
    (rename (only (ironscheme) error) (error r6rs:error))
    (srfi :8)
    (srfi :9))
  
  (define (syntax-error message history . irritants)
    (syntax-violation #f message #f #f))
    
  (define classify-error syntax-error)
  
  (define (error msg . irritants)
    (apply r6rs:error #f msg irritants))

  (include "compiler/riaxpander/history.ss")
  (include "compiler/riaxpander/closure.ss")
  (include "compiler/riaxpander/denotation.ss")
  (include "compiler/riaxpander/environment.ss")
  (include "compiler/riaxpander/transform.ss")
  (include "compiler/riaxpander/taxonomy.ss")
  (include "compiler/riaxpander/classify.ss")
  (include "compiler/riaxpander/standard.ss")
  (include "compiler/riaxpander/synrules.ss")
  (include "compiler/riaxpander/sexp.ss")
    
)    