;; default init script for IronScheme

(load "core.scm")
(define pretty-print write)
(load "genwrite.scm")

(define (load-system)
  (define (pload file)
    (load file)
    (display "."))

  (display "loading system .")
  ;(pload "srfi-23.scm") ; error - builtin

  ;; load core macros
  ;(pload "core.scm")
  
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
  
  (display " done.")
  (newline)
)

;; load some additional console helpers 
(load "IronScheme.Console.exe")
(load "IronScheme.Clr.dll")

;; this wil call load immediately, use with care :p
;; this is usefull for loading references at compile-time
;; there is a side-effect running this in the interpreter, it will be dispatched twice
(define load-diags
  (macro () `,(load "IronScheme.Diagnostics.dll")))
  

(define (run-tests) (load "tests/test.scm"))
(define (r4rs-tests) (load "tests/r4rstest.scm"))

(define r6rs-input "hello-world.ss")

(define (load-r6rs fn)
  (set! r6rs-input fn)
  (load "ironscheme-r6rs.ss"))

(define (psyntax-build)  
  (set! r6rs-input "psyntax/psyntax-buildscript.ss")
  (load "psyntax-r6rs.ss"))
  
(define (ironscheme-build)  
  (set! r6rs-input "ironscheme-buildscript.ss")
  (load "ironscheme-r6rs.ss"))

(define eval-r6rs #f)

(define (init-r6rs)
  (set! eval-r6rs (load-r6rs "r6rs-init.ss")))
  

