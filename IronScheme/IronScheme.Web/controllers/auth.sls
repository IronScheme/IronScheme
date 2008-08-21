(library (controllers auth)
  (export 
    login
    logout
    dologin)
  (import
    (ironscheme)
    (ironscheme web)
    (prefix (views auth) view-)
    (ironscheme web controllers))
    
  (define-action (login)
    (view-login))
    
  (define-action (dologin username password)
    (forms-authentication-login username)
    (let ((ru (querystring "returnUrl")))
      (if ru
        (redirect ru)
        (redirect "blog"))))
    
  (define-action (logout)
    (forms-authentication-logout)
    (redirect "blog"))    
)