(library (ironscheme random)
  (export
    make-random
    next-fixnum
    next-bytevector
    next-flonum 
    )
  (import 
    (rnrs)
    (ironscheme clr))

  (clr-using system)
  
  (define make-random
    (case-lambda
      [()       (clr-new random)]
      [(seed)   (clr-new random seed)]))
      
  (define next-fixnum
    (case-lambda
      [(rg)         (clr-call random next rg)]
      [(rg max)     (clr-call random next rg max)]
      [(rg min max) (clr-call random next rg min max)]))

  (define (next-bytevector rg bytevector)
    (clr-call random nextbytes rg bytevector))
          
  (define (next-flonum rg)
    (clr-call random nextdouble rg))
    
  (clr-clear-usings)
)