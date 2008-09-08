(library (controllers auth)
  (export 
    login
    logout)
  (import
    (ironscheme)
    (ironscheme web)
    (prefix (views auth) view-)
    (ironscheme web controllers))
    
  (define-action login
    [(get) 
      (view-login)]
    [(post username password)
      (forms-authentication-login username)
      (redirect (or (querystring "returnUrl") "blog"))])
    
  (define-action (logout)
    (forms-authentication-logout)
    (redirect "blog"))    
)