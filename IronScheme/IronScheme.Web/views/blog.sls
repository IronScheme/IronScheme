(library (views blog)
  (export
    index
    entry
    edit
    add)
  (import
    (ironscheme)
    (models blog)
    (ironscheme web)
    (ironscheme web views))
          
  (define (page-template . body)
    `(html (xmlns . "http://www.w3.org/1999/xhtml")
           (xmlns:v . "urn:schemas-microsoft-com:vml")
        (head
          (title "Blog in IronScheme")
          ,(css-link "/blog.css"))
        (body . ,body)))
        
  (define (display-entry e)
    `(v:roundrect (arcsize . ".1") (fillcolor . "#C3D9FF") (strokecolor . "#C3D9FF") (class . "blog-entry")
        (div ,(action-link/id (blog-entry-subject e) "entry" (blog-entry-id e)))
        (p ,(blog-entry-body e))
        (span (class . "blog-footer")
          "posted by " ,(blog-entry-author e) 
          " on " ,(blog-entry-date e)
          ,(when (string=? (user-name) "admin")
            `(span
              ,(action-link/id "edit" "edit" (blog-entry-id e))
              ,(action-link/id "delete" "delete" (blog-entry-id e) 
                '(onclick . "return confirm('Are you sure?')") )))
          )))        
    
  (define (index blogdata)
    (render-doctype)
    (display-html 
      (page-template
        '(h2 "Blog in 100% IronScheme")
        `(div ,@(map display-entry blogdata))
        (if (string=? (user-name) "admin")
          (action-link "Add entry" "add")
          `(a (href . "/auth/login") "Login") ))))
    
  (define (add)
    (render-doctype)
    (display-html 
      (page-template
        '(h2 "Add entry")
        `(form (action . "/blog/save") (method . "post")
          ,(make-label/input "subject" "Subject" 'text "")
          ,(make-label/input "body" "Body" 'textarea "")
          (br)
          (input (type . "submit"))))))
    
  (define (entry e)
    (render-doctype)
    (display-html 
      (page-template
        '(h2 "Blog in 100% IronScheme")
        (display-entry e))))
          
  (define (edit e)
    (render-doctype)
    (display-html 
      (page-template
        '(h2 "Edit entry")
        `(form (action . ,(string-append "/blog/modify/" (blog-entry-id e))) (method . "post")
          ,(make-label/input "subject" "Subject" 'text (blog-entry-subject e))
          ,(make-label/input "body" "Body" 'textarea (blog-entry-body e))
          (br)
          (input (type . "submit"))))))
)  
