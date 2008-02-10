;; default init script for IronScheme

(load "core.scm")
; for pretty-print
(load "genwrite.scm")

;; load some additional console helpers 
(load "IronScheme.Console.exe")

(define r6rs-input #f)

(define command-line 
  (lambda () 
    (list "ironscheme" r6rs-input)))

(define (load-r6rs fn)
  (set! r6rs-input fn)
  (load "ironscheme.boot.pp"))

#;(define (psyntax-build)  
  (set! r6rs-input "psyntax/psyntax-buildscript.ss")
  (load "psyntax/psyntax-r6rs.ss"))
  
#;(define (ironscheme-build)  
  (set! r6rs-input "ironscheme-buildscript.ss")
  (display "old version\n")
  (load "ironscheme-r6rs.ss") ; go make coffee :)
  (if #f #f))

(define eval-r6rs #f)

(define (init-r6rs)
  (set! eval-r6rs (load-r6rs "r6rs-init.ss"))
  (if #f #f))

(if (r6rs-mode?)  
  (init-r6rs)
  (load "other/mbe.scm"))
