 (library (controllers blog)
  (export
    index
    add
    entry
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
    
  (define-action (entry id)
    (view-entry (get-entry-id id)))
    
  (define-action (save subject body)
    (add-entry subject body)
    (redirect "blog"))    
    
)        
        