(library (ironscheme r5rs)
  (export
    exact->inexact
    inexact->exact
    
    quotient
    remainder
    modulo
    
    delay
    force
    
    null-environment
    scheme-report-environment
  )
  
  (import 
    (rnrs base)
    (except (rnrs r5rs) quotient remainder modulo))
    
  (define (sign n)
    (cond 
      [(> n 0) 1]
      [(< n 0) -1]
      (else 0)))
 
  (define (quotient n1 n2)
    (* (sign n1) (sign n2) (div (abs n1) (abs n2))))
  
  (define (remainder n1 n2)
    (* (sign n1) (mod (abs n1) (abs n2))))
  
  (define (modulo n1 n2)
    (* (sign n2) (mod (* (sign n2) n1) (abs n2)))) 
)