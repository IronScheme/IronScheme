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
    (ironscheme contracts)
    (ironscheme clr))

  (clr-reference "System.Web, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a")
  (clr-using System.Web)
    
  (define/contract (url-encode s:string)
    (clr-static-call HttpUtility UrlEncode (clr-cast String s)))    
    
  (define/contract (url-decode s:string)
    (clr-static-call HttpUtility UrlDecode s))    

  (define/contract (html-encode s:string)
    (clr-static-call HttpUtility HtmlEncode s))    
    
  (define/contract (html-decode s:string)
    (clr-static-call HttpUtility HtmlDecode s))    
 
)
