(library (ironscheme web)
  (export
    context
    method
    querystring
    querystring-keys
    form
    form-keys
    header
    request
    response
    redirect
    rewrite-path
    application-item
    application-item-set!
    context-item
    context-item-set!
    session
    session-set!
    user-agent
    url-decode
    url-encode
    html-decode
    html-encode
    map-path
    resolve-url
    forms-authentication-logout
    forms-authentication-login
    request-raw-url
    http-output-port
    display-html)
  (import 
    (ironscheme)
    (ironscheme xml)
    (ironscheme web-utils)
    (ironscheme clr))

  (clr-reference "System.Web, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a")
  (clr-using system.web)
  (clr-using system.collections.specialized)
  (clr-using system.web.sessionstate)
  
  (define (context)
    (clr-static-prop-get httpcontext current))
    
  (define (request)
    (clr-prop-get httpcontext request (context)))    
    
  (define (response)
    (clr-prop-get httpcontext response (context)))     
  
  (define (method)
    (clr-prop-get httprequest httpmethod (request)))    
  
  (define (get-querystring)
    (clr-prop-get httprequest querystring (request)))

  (define (get-form)
    (clr-prop-get httprequest form (request)))
    
  (define (get-headers)
    (clr-prop-get httprequest headers (request)))
    
  (define (resolve-url vpath)
    (clr-call httpresponse ApplyAppPathModifier (response) vpath))    
    
  (define (nv-helper instance key)
    (define k (clr-indexer-get namevaluecollection instance (clr-cast system.string key)))
    (if (null? k) #f
        k))       

  (define (querystring key)
    (nv-helper (get-querystring) key))

  (define (querystring-keys)
    (clr-prop-get namevaluecollection allkeys (get-querystring)))    

  (define (form key)
    (nv-helper (get-form) key))
    
  (define (form-keys)
    (clr-prop-get namevaluecollection allkeys (get-form)))    
    
  (define (header key)
    (nv-helper (get-headers) key))
    
  (define (get-session)
    (clr-prop-get httpcontext session (context)))  
    
  (define (get-app)
    (clr-prop-get httpcontext application (context)))       

  (define (session key)
    (define k (clr-indexer-get httpsessionstate (get-session) (clr-cast system.string key)))
    (if (null? k) #f
        k))       
  
  (define (session-set! key value)
    (clr-indexer-set! httpsessionstate (get-session) (clr-cast system.string key) value)
    (void))
    
    
  (define (application-item key)
    (define k (clr-indexer-get httpapplicationstate (get-app) (clr-cast system.string key)))
    (if (null? k) #f
        k))       
  
  (define (application-item-set! key value)
    (clr-indexer-set! httpapplicationstate (get-app) (clr-cast system.string key) value)
    (void))    
    
  (define (items)
    (clr-prop-get httpcontext items (context)))       
    
  (define (context-item key)
    (hashtable-ref (items) key #f))
  
  (define (context-item-set! key value)
    (hashtable-set! (items) key value))
    
  (define (user-agent)
    (clr-prop-get httprequest useragent (request)))
    
  (define (server-util)
    (clr-prop-get httpcontext server (context)))
    
  (define (map-path p)
    (clr-call httpserverutility mappath (server-util) p))   
    
  (define (http-output-port)
    (clr-prop-get httpresponse output (response)))
    
  (define (rewrite-path path)
    (clr-call httpcontext rewritepath (context) path))    
    
  (define (redirect path)
    (clr-call httpresponse redirect (response) path))    
    
  (define (request-raw-url)
    (clr-prop-get httprequest rawurl (request)))       
    
  (define (forms-authentication-logout)
    (clr-static-call system.web.security.formsauthentication SignOut))
    
  (define (forms-authentication-login user)
    (clr-static-call system.web.security.formsauthentication SetAuthCookie user #f))      
  
  (clr-clear-usings)
  
  (define (display-html html)
    (display (->xml html) (http-output-port)))
    
)
