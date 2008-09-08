(library (controllers doc)
  (export
    index
    identifier
    library-edit
    (rename (doc:library library)))
  (import
    (ironscheme)
    (ironscheme web)
    (ironscheme web views)
    (models doc)
    (prefix (views doc) view-)
    (ironscheme web controllers))
    
  (define (parse-lib id)
    (call-with-port (open-string-input-port id) read))        
 
  (define-action (index)
    (view-index (get-libraries)))   
    
  (define-action (doc:library id sort)
    (let ((lib (parse-lib id)))
      (view-library 
        lib 
        (get-symbols lib (and sort (string->symbol sort)))
        (library-doc-description (get-library lib)))))
        
  (define-action (identifier id lib)
    (let ((lib (parse-lib lib))
          (id (string->symbol id)))
      (view-identifier 
        id
        lib 
        (get-identifier id lib))))
        
  (define-action library-edit
    [(get id)
      (let ((lib (parse-lib id)))
        (view-library-edit lib (library-doc-description (get-library lib))))]
    [(post id desc)                
      (let ((lib (parse-lib id)))
        (save-library-description lib desc)
        (redirect (action/id-url "library" lib)))])

)    