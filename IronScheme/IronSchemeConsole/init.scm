;; default init script for IronScheme

(load "core.scm")
; for pretty-print
(load "genwrite.scm")

(define system-loaded #f)

(define (load-system)
  (define (pload file)
    (load file)
    (display "."))
    
  (if (not system-loaded)
  (begin

  (display "loading system .")
  ;(pload "srfi/23.scm") ; error - builtin
  
  ;; now load Macro by Example for define-syntax
  (pload "other/mbe.scm")
  
  (pload "srfi/55.scm")  ; require-extension
  (pload "srfi/0.scm")   ; cond-expand
  (pload "srfi/7.scm")   ; program/require
  ;(pload "srfi/6.scm")  ; string ports - builtin
  (pload "srfi/8.scm")   ; receive
  (pload "srfi/9.scm")   ; define-record
  (pload "srfi/11.scm")  ; let-values
  ;(pload "srfi/15.scm") ; fluid-let - to do
  (pload "srfi/16.scm")  ; case-lambda
  (pload "srfi/26.scm")  ; cut
  (pload "srfi/28.scm")  ; format
  (pload "srfi/31.scm")  ; rec
  (pload "srfi/37.scm")  ; argument-fold
  (pload "srfi/39.scm")  ; make-parameter

  ;(pload "srfi/1.scm")   ; list library
  ;(pload "srfi/35.scm")  ; conditions

  (pload "other/macros.scm")   ; commonly defined macro's
  (pload "other/pregexp.scm")  ; regex
  
  (display " done.")
  (newline)
  (set! system-loaded #t)
  ))
)

;; load some additional console helpers 
(load "IronScheme.Console.exe")

(define (run-tests) 
  (load-system)
  (load "tests/test.scm"))
  
(define (r4rs-tests) 
  (load "tests/r4rstest.scm"))

(define r6rs-input #f)

(define (load-r6rs fn)
  (set! r6rs-input fn)
  (load "ironscheme-r6rs.ss"))

(define (psyntax-build)  
  (set! r6rs-input "psyntax/psyntax-buildscript.ss")
  (load "psyntax-r6rs.ss"))
  
(define (ironscheme-build)  
  (set! r6rs-input "ironscheme-buildscript.ss")
  (display "old version\n")
  (load "ironscheme-r6rs.ss") ; go make coffee :)
  (if #f #f))

(define eval-r6rs #f)

(define (init-r6rs)
  (set! eval-r6rs (load-r6rs "r6rs-init.ss"))
  (if #f #f))
  
(init-r6rs)
