#| License
Copyright (c) 2007-2014 Llewellyn Pritchard 
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
    promise?
    
    null-environment
    scheme-report-environment)
  
  (import 
    (rnrs)
    (ironscheme contracts)
    (ironscheme records printer)
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
    
  (define-record-type (promise create-promise promise?) 
    (fields 
      (mutable ready?)
      (mutable result)
      proc))
    
  (define (make-promise proc)
    (letrec ((promise 
              (create-promise
                #f
                #f
                (lambda ()
                  (let ((promise promise)) ; compiler hack
                    (if (promise-ready? promise)
                        (promise-result promise)
                        (let ((x (proc)))
                          (if (promise-ready? promise)
                              (promise-result promise)
                              (begin (promise-ready?-set! promise #t)
                                     (promise-result-set! promise x)
                                     (promise-result promise))))))))))
      promise))
                           
  (define (force promise)
    ((promise-proc promise)))
    
  (add-record-printer! promise? 
    (lambda (x p wr)
      (display "#<promise>" p))))