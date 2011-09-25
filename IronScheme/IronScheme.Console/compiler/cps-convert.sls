(library (compiler cps-convert)
  (export sexp->cps parse->cps)
  (import 
    (except (ironscheme) define-record-type)
    (as-match)
    (srfi :9))

  (include "compiler/cps/cps.ss")
  (include "compiler/cps/cps-sexp-target.ss")
  (include "compiler/cps/sexp-to-cps.ss")

  (define (parse->cps e var)      
    ((sexp->cps e) (variable-continuator var))))