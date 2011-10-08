(library (source-optimizer prelex)
  (export 
    prelex?
    make-prelex
    prelex-name
    prelex-operand
    prelex-source-referenced?
    prelex-source-assigned?
    prelex-source-singly-referenced?
    prelex-residual-referenced?
    prelex-residual-assigned?
    prelex-residual-singly-referenced?
    prelex-global-location
    set-prelex-operand!
    set-prelex-source-referenced?!
    set-prelex-source-assigned?!
    set-prelex-source-singly-referenced?!
    set-prelex-residual-referenced?!
    set-prelex-residual-assigned?!
    set-prelex-residual-singly-referenced?!
    set-prelex-global-location!)
  (import
    (rnrs)
    (source-optimizer helpers))
    
  (define-structure (prelex name operand)
    ([source-referenced?   #f]
     [source-assigned?     #f]
     [source-singly-referenced?   #t]
     [residual-referenced? #f]
     [residual-assigned?   #f]
     [residual-singly-referenced? #t]
     [global-location      #f])))    