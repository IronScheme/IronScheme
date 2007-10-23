
(display "Loading init.scm ... ")

;; load core macros
(load "core.scm")

;; this wil call load immediately, use with care :p
;; this is usefull for loading references at compile-time
(define load-diags
  (macro () `,(load "IronScheme.Diagnostics.dll")))
  









(display "done.")
(newline)