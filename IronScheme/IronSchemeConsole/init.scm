
(display "Loading init.scm .")

;; load some additional console helpers 
(load "IronScheme.Console.exe")

(display ".")

;; load core macros
(load "core.scm")

(display ".")

;; now load Macro by Example for define-syntax
(load "mbe.scm")

(display ".")

;; this wil call load immediately, use with care :p
;; this is usefull for loading references at compile-time
;; there is a side-effect running this in the interpreter, it will be dispatched twice
(define load-diags
  (macro () `,(load "IronScheme.Diagnostics.dll")))
  





;(define-macro (run-tests) `,(load "test.scm"))



(display " done.")
(newline)


