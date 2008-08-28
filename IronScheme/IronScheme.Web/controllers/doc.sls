(library (controllers doc)
  (export
    index
    (rename (doc:library library)))
  (import
    (ironscheme)
    (ironscheme web)
    (models doc)
    (prefix (views doc) view-)
    (ironscheme web controllers))
 
  (define-action (index)
    (view-index (get-libraries)))   
    
  (define-action (doc:library id sort)
    (let ((lib (call-with-port (open-string-input-port id) read)))
      (view-library lib (get-symbols lib (and sort (string->symbol sort))))))
)    