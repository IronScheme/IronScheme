(library (models doc)
  (export
    get-doc
    get-symbols)
  (import
    (ironscheme)
    (ironscheme linq))
 
  (define (get-doc id) #f) 
  
    
  
  (define (get-symbols lib)
    (from i in (environment-symbols (environment lib))
     orderby i
     select i))

)    