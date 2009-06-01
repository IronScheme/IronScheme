(library (ironscheme programs)
  (export
    exit)
    
  (import 
    (except (rnrs) exit)
    (ironscheme clr))
  
  (define exit
    (case-lambda
      [() (exit 0)]
      [(reason)
        (let ((r (or (and (not reason) 1) reason)))
          (unless (fixnum? r)
            (assertion-violation 'exit "not an integer" r))
          (clr-static-call Environment Exit r))]))
        
)