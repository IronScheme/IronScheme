(library (ironscheme mutable-pairs)
  (export
    set-car!
    set-cdr!)
    
  (import 
    (rnrs)
    (ironscheme clr))
    
  (define (set-car! lst val)
    (unless (pair? lst)
      (assertion-violation 'set-car! "not a pair" val))
    (clr-field-set! IronScheme.Runtime.Cons car lst val))      
    
  (define (set-cdr! lst val)
    (unless (pair? lst)
      (assertion-violation 'set-cdr! "not a pair" val))
    (clr-field-set! IronScheme.Runtime.Cons cdr lst val))      
  
)

