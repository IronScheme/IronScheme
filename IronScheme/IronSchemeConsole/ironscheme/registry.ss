(library (ironscheme registry)
  (export
    get-registry-value
    set-registry-value!
    )
  (import 
    (rnrs)
    (ironscheme clr))

  (clr-using microsoft.win32)

  (define (get-registry-value key name default)
    (clr-static-call registry getvalue key name default))    

  (define (set-registry-value! key name value)
    (clr-static-call registry setvalue key name value))    
    
 ;; todo: registry keys
  
  (clr-clear-usings)
)