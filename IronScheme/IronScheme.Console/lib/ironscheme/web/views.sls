#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme web views)
  (export
    css-link
    javascript
    javascript-include
    render-doctype
    action-url
    action/id-url
    action-link
    action/id-link
    make-label/input
    define-view)
  (import
    (ironscheme)
    (ironscheme web))

  (define (render-doctype)
    (display "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \
       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n" (http-output-port)))  
                    
  (define (css-link href)
    `(link (rel . stylesheet) (type . text/css) (href . ,(resolve-url href)))) 
    
  (define (javascript-include href)
    `(script (type . text/javascript) (src . ,(resolve-url href)) ""))     
    
  (define (javascript script)
    `(script (type . text/javascript) ,script))
    
  (define action-url
    (case-lambda 
      [(action)
        (action-url (context-item 'controller) action)]
      [(controller action)
        (resolve-url 
          (if (or (string-ci=? action "index") (string=? action ""))
              (string-append "~/" controller)
              (string-append "~/" controller "/" action)))]))

  (define action/id-url
    (case-lambda 
      [(action id)
        (action/id-url (context-item 'controller) action id)]
      [(controller action id)
        (string-append (action-url controller action) "?" (url-encode (format "~a" id)))]))
    
  (define (action-link name action . args)
    `(a (href . ,(action-url action)) ,@args ,name))
    
  (define (action/id-link name action id . args)
    `(a (href . ,(action/id-url action id)) ,@args ,name))    
    
  (define (make-label/input id label type value)
    `(div 
       (label (for . ,id) ,label) 
       (input (type . ,type) (name . ,id) (value . ,(or value "")))))
          
  (define-syntax define-view
    (syntax-rules ()
      [(_ (n . args) body body* ...)
        (define (n . args)
          (context-item-set! 'view 'n)
          (render-doctype)
          (display-html
            (begin body body* ...)))])))    
