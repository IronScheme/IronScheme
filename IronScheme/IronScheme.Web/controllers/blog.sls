(library (controllers blog)
  (export
    index
    previous
    add
    edit
    entry
    delete
    search)
  (import
    (ironscheme)
    (ironscheme web)
    (ironscheme web views)
    (models blog)
    (prefix (views blog) view-)
    (ironscheme web controllers))
    
  (define page-size 5)    
    
  (define-action (index)
    (view-index (get-data 0 page-size) 0))
    
  (define-action (previous id)
    (let ((index (string->number id)))
      (view-index (get-data index page-size) index)))
    
  (define-action add
    [(get) 
      (view-add)]
    [(post subject body)
      (let ((e (add-entry subject body)))
        (redirect (action/id-url "entry" (blog-entry-id e))))])
    
  (define-action (search searchterm)
    (view-index (search-data searchterm) 0))    

  (define-action edit
    [(get id) 
      (view-edit (get-entry-id (string->number id)))]
    [(post id subject body)
      (edit-entry! (string->number id) subject body) 
      (redirect (action/id-url "entry" id))])

  (define-action (delete id)
    (delete-entry! (string->number id))
    (redirect (action-url "index")))
    
  (define-action (entry id)
    (view-entry (get-entry-id (string->number id))))
    
)        
        
