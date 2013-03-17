#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme web-utils)
  (export
    url-decode
    url-encode
    html-decode
    html-encode)
  (import 
    (ironscheme)
    (ironscheme contracts)
    (ironscheme clr))

  (clr-reference System.Web)
  (clr-using System.Web)
    
  (define/contract (url-encode s:string)
    (clr-static-call HttpUtility UrlEncode (clr-cast String s)))    
    
  (define/contract (url-decode s:string)
    (clr-static-call HttpUtility UrlDecode s))    

  (define/contract (html-encode s:string)
    (clr-static-call HttpUtility HtmlEncode s))    
    
  (define/contract (html-decode s:string)
    (clr-static-call HttpUtility HtmlDecode s)))
