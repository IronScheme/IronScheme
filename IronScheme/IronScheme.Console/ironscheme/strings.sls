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

(library (ironscheme strings)
  (export 
    string-split
    string-index-of
    string-contains?
    string-starts-with?
    string-ends-with?
    string-ci-contains?
    string-ci-starts-with?
    string-ci-ends-with?    
    ;string-trim-start
    ;string-trim-end
    string-trim
    string-join
    string-replace
    )
  (import 
    (rnrs)
    (ironscheme contracts)
    (ironscheme clr))
    
  (define/contract (string-split str:string . del:string)
    (clr-call String "Split(String[])" str (list->vector del) 'none))  
    
  (define/contract (string-join del:string strs:list)
    (clr-static-call String Join del (list->vector strs)))  
    
  (define/contract (string-replace str:string old:string new:string)
    (clr-call String "Replace(String,String)" str old new))  
          
  (define/contract string-index-of
    (case-lambda
      [(str:string sub:string)
        (clr-call String "IndexOf(String)" str sub)]
      [(str:string sub:string k:fixnum)  
        (clr-call String "IndexOf(String,Int32)" str sub k)]))
    
  (define/contract (string-contains? str:string sub:string)
    (clr-call String Contains str sub))
    
  (define/contract (string-ci-contains? str:string sub:string)
    (clr-call String Contains (string-upcase str) (string-upcase sub)))

  (define/contract (string-starts-with? str:string sub:string)
    (clr-call String StartsWith str sub))

  (define/contract (string-ci-starts-with? str:string sub:string)
    (clr-call String StartsWith (string-upcase str) (string-upcase sub)))

  (define/contract (string-ends-with? str:string sub:string)
    (clr-call String EndsWith str sub))

  (define/contract (string-ci-ends-with? str:string sub:string)
    (clr-call String EndsWith (string-upcase str) (string-upcase sub)))

  (define/contract (string-trim str:string)
    (clr-call String Trim str))

          
)
  