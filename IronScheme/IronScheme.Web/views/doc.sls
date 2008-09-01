(library (views doc)
  (export 
    index
    identifier
    library-edit
    (rename (view:library library)))
  (import
    (ironscheme)
    (ironscheme linq)
    (ironscheme web)
    (ironscheme web views))
    
  (define (page-template . body)
    `(html (xmlns . "http://www.w3.org/1999/xhtml")
        (head 
          (title "Documentation for IronScheme")
          ,(css-link "~/styles/doc.css"))
        (body
          (h1 ,(action-link "Documentation for IronScheme" ""))
          . ,body)))    
          
  (define (make-lib-link lib)
    (action/id-link (format "~a" lib) "library" (format "~a" lib)))
    
  (define (flatten lst)
    (apply append lst))  
    
  (define-view (identifier id lib doc)
    (page-template
      `(h2 ,(make-lib-link lib))
      `(h3 ,id)
      doc))       
    
  (define-view (index libs)
    (apply 
      page-template
      '(h2 "Libraries")
      (flatten
        (map 
          (lambda (lib)
            `((h3 ,(car lib)) . 
              ,(map 
                (lambda (i)
                  `(h4 ,(make-lib-link i))) 
                (cdr lib)))) 
         libs))))
         
  (define-view (library-edit id desc)
    (page-template
      `(h2 (no-escape ,id))
      `(form (action . ,(action/id-url "library-save" id)) (method . post)
         (textarea (style . "width:500px;height:200px") (id . desc) (name . desc) ,desc)
         (br)
         (input (type . submit) (value . Save)))))
      
         
  (define (make-sort-link id type)
    `(a 
      (href . ,(string-append (action/id-url "library" id) "?sort=" type)) 
      ,(string-append "Sort by " type)))
      
  (define (make-id-link id lib)
    `(a 
      (href . ,(string-append 
                  (action-url "identifier") 
                  "?id=" 
                  (url-encode (format "~a" id)) 
                  "&lib=" 
                  (url-encode (format "~a" lib)))) 
      ,id))
     
  (define-view (view:library id bindings desc)
    (page-template 
      `(h2 (no-escape ,id))
      `(h4 (a (href . ,(action/id-url "library-edit" id)) "Description"))
      `(p ,desc)
      `(h4 "Exports")
      `(table (class . id-table)
        (tr
          (th ,(make-sort-link id "name"))
          (th ,(make-sort-link id "type")))
        ,@(map 
          (lambda (x)
            `(tr (td (class . ,(cdr x)) ,(make-id-link (car x) id)) (td (em ,(cdr x)))))
          bindings))))
)
