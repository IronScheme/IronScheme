(library (ironscheme web)
  (export
    http-method
    querystring
    form
    request
    session
    session-set!)
  (import (ironscheme)
    (ironscheme clr))

  (clr-reference system.web)
  (clr-using system.web)
  (clr-using system.collections.specialized)
  (clr-using system.web.sessionstate)
  
  (define (current-context)
    (clr-static-prop-get httpcontext current))
    
  (define (request)
    (clr-prop-get httpcontext request (current-context)))    
  
  (define (http-method)
    (clr-prop-get httprequest httpmethod (request)))    
  
  (define (get-querystring)
    (clr-prop-get httprequest querystring (request)))

  (define (get-form)
    (clr-prop-get httprequest form (request)))
    
  (define (nv-helper instance key)
    (define k (clr-indexer-get namevaluecollection instance (clr-cast system.string (symbol->string key))))
    (if (null? k) #f
        k))       

  (define (querystring key)
    (nv-helper (get-querystring) key))

  (define (form key)
    (nv-helper (get-form) key))
    
  (define (get-session)
    (clr-prop-get httpcontext session (current-context)))    

  (define (session key)
    (define k (clr-indexer-get httpsessionstate (get-session) (clr-cast system.string (symbol->string key))))
    (if (null? k) #f
        k))       
  
  (define (session-set! key value)
    (clr-indexer-set! httpsessionstate (get-session) (clr-cast system.string (symbol->string key)) value)
    (void))
  
  (clr-clear-usings)
    
)