(library (ironscheme web)
  (export
    method
    querystring
    form
    header
    request
    session
    session-set!
    user-agent
    url-decode
    url-encode
    html-decode
    html-encode
    map-path
    display-html
    )
  (import 
    (ironscheme)
    (ironscheme clr))

  (clr-reference system.web)
  (clr-using system.web)
  (clr-using system.collections.specialized)
  (clr-using system.web.sessionstate)
  
  (define (current-context)
    (clr-static-prop-get httpcontext current))
    
  (define (request)
    (clr-prop-get httpcontext request (current-context)))    
  
  (define (method)
    (clr-prop-get httprequest httpmethod (request)))    
  
  (define (get-querystring)
    (clr-prop-get httprequest querystring (request)))

  (define (get-form)
    (clr-prop-get httprequest form (request)))
    
  (define (get-headers)
    (clr-prop-get httprequest headers (request)))
    
    
  (define (nv-helper instance key)
    (define k (clr-indexer-get namevaluecollection instance (clr-cast system.string key)))
    (if (null? k) #f
        k))       

  (define (querystring key)
    (nv-helper (get-querystring) key))

  (define (form key)
    (nv-helper (get-form) key))
    
  (define (header key)
    (nv-helper (get-headers) key))
    
  (define (get-session)
    (clr-prop-get httpcontext session (current-context)))    

  (define (session key)
    (define k (clr-indexer-get httpsessionstate (get-session) (clr-cast system.string key)))
    (if (null? k) #f
        k))       
  
  (define (session-set! key value)
    (clr-indexer-set! httpsessionstate (get-session) (clr-cast system.string key) value)
    (void))
    
  (define (user-agent)
    (clr-prop-get httprequest useragent (request)))
    
  (define (server-util)
    (clr-prop-get httpcontext server (current-context)))
    
  (define (url-encode s)
    (clr-call httpserverutility urlencode (server-util) s))    
    
  (define (url-decode s)
    (clr-call httpserverutility urldecode (server-util) s))    

  (define (html-encode s)
    (clr-call httpserverutility htmlencode (server-util) s))    
    
  (define (html-decode s)
    (clr-call httpserverutility htmldecode (server-util) s))    
    
  (define (map-path p)
    (clr-call httpserverutility mappath (server-util) p))   
  
  
  
  (clr-clear-usings)
  
  
  (define (display-html html)
    (define (attribute? x)
      (and (pair? x) (not (or (null? (cdr x)) (pair? (cdr x))))))
    (define (string-map f l)
      (apply string-append (map f l)))    
    (define (->html html)
      (cond
        [(string? html) (html-encode html)]
        [(null? html) ""]
        [(not (pair? html)) (format "~a" html)]
        [(attribute? html)
          (let ((name (car html))
                (value (cdr html))) 
            (if (boolean? value)
              (if (eq? #t value)
                (format " ~a" name)
                "")
              (format " ~a=~s" name (html-encode (format "~a" value)))))]
        [else
          (let ((tag (car html))
                (body (cdr html)))
            (let-values ([(attrs children) (partition attribute? body)])
              (if (null? children)
                (format "<~a~a/>\n" 
                  tag
                  (string-map ->html attrs))
                (format "<~a~a>\n~a\n</~a>\n" 
                  tag
                  (string-map ->html attrs) 
                  (string-map ->html children)
                  tag))))]))
    (display (->html html)))
    
)