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
          
  (define (render-doctype)
    (display "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \
                    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n" (http-output-port)))  
                    
  (define (css-link href)
    `(link (rel . "stylesheet") (type . "text/css") (href . ,(resolve-url href)))) 
    
  (define (make-action-url action)
    (resolve-url (string-append "~/" (context-item 'controller) "/" action)))
    
  (define (make-action/id-url action id)
    (string-append (make-action-url action) "/" id))    
    
  (define (action-link name action . args)
    `(a (href . ,(make-action-url action)) ,@args ,name))
    
  (define (action-link/id name action id . args)
    `(a (href . ,(make-action/id-url action id)) ,@args ,name))    
    
  (define (make-label/input id label type value)
    `(div (label (for . ,id) ,label) 
      ,(case type
         ((text password) `(input (type . ,type) (name . ,id) (value . ,value)))
         ((textarea) `(textarea (name . ,id) ,value)))))    
          
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
        (span 
          "posted by " ,(blog-entry-author e) 
          " on " ,(blog-entry-date e)
          ,(action-link/id "edit" "edit" (blog-entry-id e))
          ,(action-link/id "delete" "delete" (blog-entry-id e) '(onclick . "return confirm('Are you sure?')") )
          )))        
    
  (define (index blogdata)
    (render-doctype)
    (display-html 
      (page-template
        '(h2 "Blog in 100% IronScheme")
        `(div ,@(map display-entry blogdata))
        (action-link "Add entry" "add"))))
    
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
