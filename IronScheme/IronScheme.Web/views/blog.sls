(library (views blog)
  (export
    index
    entry
    edit
    add)
  (import
    (ironscheme)
    (models blog)
    (ironscheme web))
    
  (define (display-entry e)
    `(div (class . "blog-entry")
        (div (a (href . ,(string-append "/blog/entry/" (blog-entry-id e))) 
          ,(blog-entry-subject e)))
        (p ,(blog-entry-body e))
        (span 
          "posted by " ,(blog-entry-author e) 
          " on " ,(blog-entry-date e)
          (a (href . ,(string-append "/blog/edit/" (blog-entry-id e))) "edit" )
          (a (href . ,(string-append "/blog/delete/" (blog-entry-id e)))
             (onclick . "return confirm('Are you sure?')") "delete" )
          )))
          
  (define (page-template . body)
    `(html
        (head
          (title "Blog in IronScheme")
          (link (rel . "stylesheet") (type . "text/css") (href . "/blog.css")))
        (body . ,body)))
    
  (define (index blogdata)
    (display-html 
      (page-template
        '(h2 "Blog in 100% IronScheme")
        `(div ,@(map display-entry blogdata))
        '(a (href . "/blog/add") "Add entry"))))
    
  (define (make-label/input id label type value)
    `(p (label (for . ,id) ,label) 
      ,(case type
         (("text") `(input (type . ,type) (name . ,id) (value . ,value)))
         (("textarea") `(textarea (name . ,id) ,value)))))
    
  (define (add)
    (display-html 
      (page-template
        '(h2 "Add entry")
        `(form (action . "/blog/save") (method . "post")
          ,(make-label/input "subject" "Subject" "text" "")
          ,(make-label/input "body" "Body" "textarea" "")
          (br)
          (input (type . "submit"))))))
    
  (define (entry e)
    (display-html 
      (page-template
        '(h2 "Blog in 100% IronScheme")
        (display-entry e))))
          
  (define (edit e)
    (display-html 
      (page-template
        '(h2 "Edit entry")
        `(form (action . ,(string-append "/blog/modify/" (blog-entry-id e))) (method . "post")
          ,(make-label/input "subject" "Subject" "text" (blog-entry-subject e))
          ,(make-label/input "body" "Body" "textarea" (blog-entry-body e))
          (br)
          (input (type . "submit"))))))
)  
