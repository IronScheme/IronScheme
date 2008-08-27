(library (views doc)
  (export 
    index
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
          ,(css-link "~/doc.css"))
        (body
          (h1 "Documentation for IronScheme")
          . ,body)))    
          
  (define (make-lib-link lib)
    (action/id-link (format "~a" lib) "library" (format "~a" lib)))
    
  (define (flatten lst)
    (apply append lst))    
    
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
     
  (define-view (view:library id bindings)
    (page-template 
      `(h2 (no-escape ,id))
      `(table (class . id-table)
        ,@(map 
          (lambda (x)
            `(tr (td (class . ,(cdr x)) ,(car x)) (td (em ,(cdr x)))))
          bindings))))
)
