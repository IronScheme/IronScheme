#| License
Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme r5rs)
  (export
    exact->inexact
    inexact->exact
    
    quotient
    remainder
    modulo
    
    delay
    force
    make-promise
    
    null-environment
    scheme-report-environment
  )
  
  (import 
    (rnrs)
    (ironscheme contracts)
    (only (rnrs r5rs) 
      delay    
      null-environment
      scheme-report-environment))
    
  (define inexact->exact exact)    
  (define exact->inexact inexact)
    
  (define/contract (sign n:real)
    (cond 
      [(> n 0) 1]
      [(< n 0) -1]
      (else 0)))
 
  (define/contract (quotient n1:integer n2:integer)
    (* (sign n1) (sign n2) (div (abs n1) (abs n2))))
  
  (define/contract (remainder n1:integer n2:integer)
    (* (sign n1) (mod (abs n1) (abs n2))))
  
  (define/contract (modulo n1:integer n2:integer)
    (* (sign n2) (mod (* (sign n2) n1) (abs n2)))) 
    
  (define make-promise
    (lambda (proc)
      (let ((result-ready? #f)
            (result #f))
        (lambda ()
          (if result-ready?
              result
              (let ((x (proc)))
                (if result-ready?
                    result
                    (begin (set! result-ready? #t)
                           (set! result x)
                           result))))))))
                           
  (define force
    (lambda (object)
      (object)))                           
)