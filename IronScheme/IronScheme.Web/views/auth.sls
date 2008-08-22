(library (views auth)
  (export 
    login)
  (import
    (ironscheme)
    (ironscheme web)
    (ironscheme web views))
    
  (define (page-template . body)
    `(html (xmlns . "http://www.w3.org/1999/xhtml")
        (head 
          (title "Blog in IronScheme")
          ,(css-link "~/blog.css"))
        (body . ,body)))    
    
  (define (login)
    (render-doctype)
    (let ((ru (querystring "returnUrl")))
      (display-html 
        (page-template
          '(h2 "Login")
          `(form (action . ,(string-append "/auth/dologin" 
                              (if ru (string-append "?returnUrl=" ru) "" ))) 
                 (method . post)
            ,(make-label/input "username" "Username" "text" "")
            ,(make-label/input "password" "Password" "password" "")
            (br)
            (input (type . submit) (value . Login)))))))
)
