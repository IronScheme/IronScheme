(library (models doc)
  (export
    get-doc
    get-symbols)
  (import
    (ironscheme)
    (ironscheme linq))
 
  (define (get-doc id) #f) 
  
    
  
  (define (get-symbols lib)
    (from i in (environment-bindings (environment lib))
     orderby (car i)
     select i))

)    