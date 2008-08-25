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
    (view-index))   
    
  (define-action (doc:library id)
    (let ((lib (call-with-port (open-string-input-port id) read)))
      (view-library lib (get-symbols lib))))
)    