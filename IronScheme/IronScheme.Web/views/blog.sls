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
    `(html (xmlns . http://www.w3.org/1999/xhtml)
        (head
          (title "Blog in IronScheme")
          ,(css-link "~/styles/blog.css"))
        (body
          ,(display-menu)
          . ,body)))
          
  (define (edit-page-template . body)
    (apply 
      page-template
      (javascript-include "~/scripts/wysiwyg.js")
      (javascript-include "~/scripts/wysiwyg-settings.js")
      (javascript "
      var e = new WYSIWYG.Settings();  
      e.ImagesDir = '/' + e.ImagesDir;
      e.CSSFile = '/' + e.CSSFile;
      e.PopupsDir = '/' + e.PopupsDir;
      WYSIWYG.attach('all', e);        
      ")
      body))
        
  (define (display-menu)
    `(ul (class . menu)
      (li ,(action-link "Home" ""))
      ,(if (string=? (user-name) "admin")
        `(li ,(action-link "Add entry" "add")))
      ,(if (user-authenticated?)
        '(li (a (href . "/auth/logout") "Logout"))
        '(li (a (href . "/auth/login") "Login")))
      (li
        (form (action . ,(action-url "search")) (method . post)
          (input (type . text) (name . searchterm) (value . ,(or (form "searchterm") "")))
          (input (type . submit) (value . Search))))        
     ))
        
  (define (display-entry e)
    (let ((id (blog-entry-id e)))
    `(div (class . blog-entry)
        (div (class . blog-header) ,(action/id-link (blog-entry-subject e) "entry" id))
        (div (class . blog-body) (no-escape ,(blog-entry-body e)))
        (span (class . blog-footer)
          "posted by " ,(blog-entry-author e) 
          " on " ,(blog-entry-date e)
          ,(when (string=? (user-name) "admin")
            `(span
              ,(action/id-link "edit" "edit" id)
              ,(action/id-link "delete" "delete" id 
                '(onclick . "return confirm('Are you sure?')") )))
          ))))
    
  (define-view (index blogdata pageindex)
    (page-template
      '(h2 "Blog in 100% IronScheme")
      `(div ,@(map display-entry blogdata))
      (if (not (string=? "search" (context-item 'action)))
        `(span          
          ,(if (= pageindex 1)
            (action-link "<<" "index"))
          ,(if (> pageindex 1)
            (action/id-link "<<" "previous" (- pageindex 1)))
          ,(if (not (null? blogdata))
            (action/id-link ">>" "previous" (+ pageindex 1)))))
      ))
    
  (define-view (add)
    (edit-page-template
      '(h2 "Add entry")
      `(form (action . ,(action-url "add")) (method . post)
        ,(make-label/input "subject" "Subject" 'text "")
        (textarea (style . "width:500px;height:200px") (name . body) (id . body) "")
        (br)
        (input (type . submit)))))
    
  (define-view (entry e)
    (page-template
      '(h2 "Blog in 100% IronScheme")
      (display-entry e)))
          
  (define-view (edit e)
    (edit-page-template
      '(h2 "Edit entry")
      `(form (action . ,(action/id-url "edit" (blog-entry-id e))) (method . post)
        ,(make-label/input "subject" "Subject" 'text (blog-entry-subject e))
        (textarea (style . "width:500px;height:200px") (name . body) (id . body) ,(blog-entry-body e))
        (br)
        (input (type . submit)))))
)  
