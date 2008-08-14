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
    interaction-environment
  )
  
  (import 
    (rnrs)
    (except (rnrs r5rs) quotient remainder modulo force))
    
  (define (sign n)
    (cond 
      [(> n 0) 1]
      [(< n 0) -1]
      (else 0)))
 
  (define (quotient n1 n2)
    (unless (integer? n1)
      (assertion-violation 'quotient "not an integer" n1))
    (unless (integer? n2)
      (assertion-violation 'quotient "not an integer" n2))
    (* (sign n1) (sign n2) (div (abs n1) (abs n2))))
  
  (define (remainder n1 n2)
    (unless (integer? n1)
      (assertion-violation 'remainder "not an integer" n1))
    (unless (integer? n2)
      (assertion-violation 'remainder "not an integer" n2))
    (* (sign n1) (mod (abs n1) (abs n2))))
  
  (define (modulo n1 n2)
    (unless (integer? n1)
      (assertion-violation 'modulo "not an integer" n1))
    (unless (integer? n2)
      (assertion-violation 'modulo "not an integer" n2))
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