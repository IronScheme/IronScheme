(library (views web-repl)
  (export
    index
    result)
  (import
    (ironscheme)
    (ironscheme web)
    (ironscheme web views))
  
  (define (page-template . body)
    `(html (xmlns . "http://www.w3.org/1999/xhtml")
        (head (title "web-repl"))
        (body . ,body)))
          
  (define (result expr)
    (display-html `(pre ,(format "~a" expr))))
    
  (define-view (index)
    (page-template 
      `(form (action . ,(action-url "index")) (method . post)
        (textarea (style . "width:500px;height:200px") (name . expr) (id . expr) "")
        (br)
        (input (type . submit)))))
)