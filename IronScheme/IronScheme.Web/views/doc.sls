(library (views doc)
  (export 
    index
    identifier
    library-edit
    (rename (view:library library)))
  (import
    (ironscheme)
    (ironscheme web)
    (ironscheme web views)
    (ironscheme record-case)
    (models doc))
    
  (define (page-template . body)
    `(html (xmlns . "http://www.w3.org/1999/xhtml")
        (head 
          (title "Documentation for IronScheme")
          ,(css-link "~/styles/doc.css"))
        (body
          (h1 ,(action-link "Documentation for IronScheme" ""))
          . ,body)))    
          
  (define (make-lib-link lib)
    (let ((lib (format "~a" lib)))
      (action/id-link lib "library" lib)))
    
  (define (flatten lst)
    (apply append lst))
    
  (define (render-form name)
    (lambda (form)
    `(p ,(format "~a" (cons name (cdr form))))))    
    
  (define (render-doc doc)
    (record-case doc
      [(proc-id-doc name forms) `(div ,@(map (render-form name) forms))]
      [(identifier-doc) doc]
      [else doc]))      
    
  (define-view (identifier id lib doc)
    (page-template
      `(h2 ,(make-lib-link lib))
      `(h3 ,id)
      (render-doc doc)))       
    
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
      `(h2 ,(format "~a" id))
      `(form (action . ,(action/id-url "library-edit" id)) (method . post)
         (textarea (style . "width:500px;height:200px") (id . desc) (name . desc) ,desc)
         (br)
         (input (type . submit) (value . Save)))))
      
         
  (define (make-sort-link id type)
    `(a 
      (href . ,(string-append (action/id-url "library" id) "&sort=" type)) 
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
      `(h2 ,(format "~a" id))
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
