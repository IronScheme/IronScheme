#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

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
