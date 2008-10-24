(library (ironscheme web-utils)
  (export
    url-decode
    url-encode
    html-decode
    html-encode)
  (import 
    (ironscheme)
    (ironscheme clr))

  (clr-reference "System.Web, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a")
  (clr-using system.web)
    
  (define (url-encode s)
    (clr-static-call httputility urlencode (clr-cast system.string s)))    
    
  (define (url-decode s)
    (clr-static-call httputility urldecode s))    

  (define (html-encode s)
    (clr-static-call httputility htmlencode s))    
    
  (define (html-decode s)
    (clr-static-call httputility htmldecode s))    

  
    
)
