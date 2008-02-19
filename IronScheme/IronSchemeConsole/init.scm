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

(define eval-r6rs #f)

(define (init-r6rs)
  (set! eval-r6rs (load-r6rs "r6rs-init.ss"))
  (if #f #f))

(init-r6rs)
 
