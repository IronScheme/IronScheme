(library (ironscheme web views)
  (export
    css-link
    render-doctype
    make-action-url
    make-action/id-url
    action-link
    action-link/id
    make-label/input)
  (import
    (ironscheme)
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
         ((textarea) `(textarea (name . ,id) ,value))
         (else `(input (type . ,type) (name . ,id) (value . ,value))))))        
    
)    