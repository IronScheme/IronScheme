(library (views auth)
  (export 
    login)
  (import
    (ironscheme)
    (ironscheme web)
    (ironscheme web controllers))
    
  (define (page-template . body)
    `(html
        (head
          (title "Blog in IronScheme")
          (link (rel . "stylesheet") (type . "text/css") (href . "/blog.css")))
        (body . ,body)))    
    
  (define (make-label/input id label type value)
    `(p (label (for . ,id) ,label) 
      ,(case type
         (("text" "password") `(input (type . ,type) (name . ,id) (value . ,value)))
         (("textarea") `(textarea (name . ,id) ,value)))))    
    
  (define (login)
    (display-html 
      (page-template
        '(h2 "Login")
        `(form (action . ,(string-append "/auth/dologin?returnUrl=" (querystring "returnUrl"))) (method . "post")
          ,(make-label/input "username" "Username" "text" "")
          ,(make-label/input "password" "Password" "password" "")
          (br)
          (input (type . "submit") (value . "Login"))))))
)
