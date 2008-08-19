(library (controllers blog)
  (export
    index
    add
    edit
    modify
    entry
    delete
    save)
  (import
    (ironscheme)
    (ironscheme web)
    (models blog)
    (prefix (views blog) view-)
    (ironscheme web controllers))
    
  (define-action (index)
    (view-index (get-data)))
    
  (define-action (add)
    (view-add))

  (define-action (edit id)
    (view-edit (get-entry-id (string->number id))))

  (define-action (delete id)
    (delete-entry! (string->number id))
    (redirect "blog"))
    
  (define-action (entry id)
    (view-entry (get-entry-id (string->number id))))
    
  (define-action (save subject body)
    (add-entry subject body)
    (redirect "blog"))    

  (define-action (modify id subject body)
    (edit-entry! (string->number id) subject body) 
    (redirect "blog"))
    
)        
        