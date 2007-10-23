
(display "Loading init.scm ... ")

;; load core macros
(load "core.scm")

;; this wil call load immediately, use with care :p
;; this is usefull for loading references at compile-time
;; there is a side-effect running this in the interpreter, it will be dispatched twice
(define load-diags
  (macro () `,(load "IronScheme.Diagnostics.dll")))
  









(display "done.")
(newline)