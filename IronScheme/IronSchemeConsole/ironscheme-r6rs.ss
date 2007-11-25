

(define (open-string-output-port)
  (let ((p (open-output-string)))
    (values p (lambda () (get-output-string p))))) 

(define command-line 
  (lambda () 
    (list "ironscheme" r6rs-input))) ; defined in init.scm
    
(load "ironscheme.boot.pp")
