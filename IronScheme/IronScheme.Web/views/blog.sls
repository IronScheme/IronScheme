(library (views blog)
  (export
    index
    entry
    add)
  (import
    (ironscheme)
    (models blog)
    (ironscheme web))
    
  (define (display-entry e)
    (display-html 
      `(div
          (h3 (a (href . ,(string-append "blog/entry/" (blog-entry-subject e))) 
            ,(blog-entry-subject e)))
          (p ,(blog-entry-body e))
          (span 
            "posted by " ,(blog-entry-author e) 
            " on " ,(blog-entry-date e) ))))        
    
  (define (index blogdata)
    (display-html '(h2 "IronScheme Blog in IronScheme"))
    (for-each display-entry blogdata)
    (display-html '(a (href . "blog/add") "Add entry")))
    
  (define (make-label/input id label type)
    `(p (label (for . ,id) ,label) 
      ,(case type
         (("text") `(input (type . ,type) (name . ,id)))
         (("textarea") `(textarea (name . ,id) "")))))
    
  (define (add)
    (display-html '(h2 "Add entry"))
    (display-html 
      `(form (action . "save") (method . "post")
        ,(make-label/input "subject" "Subject" "text")
        ,(make-label/input "body" "Body" "textarea")
         (input (type . "submit")))))
    
  (define (entry e)
    (display-entry e))
)  
