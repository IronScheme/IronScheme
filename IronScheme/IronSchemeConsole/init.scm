;; default init script for IronScheme
(display "Loading init.scm .")


((lambda ()
  (define (pload file)
    (load file)
    (display "."))

  ;(pload "srfi-23.scm") ; error - builtin

  ;; load core macros
  (pload "core.scm")

  ;; now load Macro by Example for define-syntax
  (pload "mbe.scm")
  
  (pload "srfi-55.scm")  ; require-extension
  (pload "srfi-0.scm")   ; cond-expand
  (pload "srfi-7.scm")   ; program/require
  ;(pload "srfi-6.scm")  ; string ports - builtin
  (pload "srfi-8.scm")   ; receive
  (pload "srfi-9.scm")   ; define-record
  (pload "srfi-11.scm")  ; let-values
  ;(pload "srfi-15.scm") ; fluid-let - to do
  (pload "srfi-16.scm")  ; case-lambda
  (pload "srfi-26.scm")  ; cut
  (pload "srfi-28.scm")  ; format
  (pload "srfi-31.scm")  ; rec
  (pload "srfi-37.scm")  ; argument-fold
  (pload "srfi-39.scm")  ; make-parameter

  (pload "srfi-1.scm")   ; list library
  (pload "srfi-35.scm")  ; conditions

  (pload "genwrite.scm") ; pretty-print
  
  (pload "macros.scm")   ; commonly defined macro's
  (pload "pregexp.scm")  ; regex
  
  ;; load some additional console helpers 
  (pload "IronScheme.Console.exe")
))


;; this wil call load immediately, use with care :p
;; this is usefull for loading references at compile-time
;; there is a side-effect running this in the interpreter, it will be dispatched twice
(define load-diags
  (macro () `,(load "IronScheme.Diagnostics.dll")))
  

(define (run-tests) (load "test.scm"))
(define (r4rs-tests) (load "r4rstest.scm"))



(display " done.")
(newline)


