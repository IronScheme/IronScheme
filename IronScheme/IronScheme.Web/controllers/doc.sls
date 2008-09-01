(library (controllers doc)
  (export
    index
    identifier
    library-edit
    library-save
    (rename (doc:library library)))
  (import
    (ironscheme)
    (ironscheme web)
    (ironscheme web views)
    (models doc)
    (prefix (views doc) view-)
    (ironscheme web controllers))
 
  (define-action (index)
    (view-index (get-libraries)))   
    
  (define-action (doc:library id sort)
    (let ((lib (call-with-port (open-string-input-port id) read)))
      (view-library 
        lib 
        (get-symbols lib (and sort (string->symbol sort)))
        (library-doc-description (get-library lib)))))
        
  (define-action (identifier id lib)
    (let ((lib (call-with-port (open-string-input-port lib) read))
          (id (string->symbol id)))
      (view-identifier 
        id
        lib 
        (get-identifier id lib))))
        
  (define-action (library-edit id)
    (let ((lib (call-with-port (open-string-input-port id) read)))
      (view-library-edit lib (library-doc-description (get-library lib)))))        

  (define-action (library-save id desc)
    (let ((lib (call-with-port (open-string-input-port id) read)))
      (save-library-description lib desc)
      (redirect (action/id-url "library" lib))))
      
)    