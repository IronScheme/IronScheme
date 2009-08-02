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
    (clr-call system.string split str (clr-cast system.string[] (list->vector del)) 'none))  
    
  (define/contract (string-join del:string strs:list)
    (clr-static-call system.string join del (list->vector strs)))  
    
  (define/contract (string-replace str:string old:string new:string)
    (clr-call system.string replace str (clr-cast system.string old) (clr-cast system.string new)))  
          
  (define string-index-of
    (case/contract
      [(str:string sub:string)    
        (clr-call system.string indexof str (clr-cast system.string sub))]
      [(str:string sub:string k:fixnum)  
        (clr-call system.string indexof str (clr-cast system.string sub) (clr-cast system.int32 k))]))
    
  (define/contract (string-contains? str:string sub:string)
    (clr-call system.string contains str sub))
    
  (define/contract (string-ci-contains? str:string sub:string)
    (clr-call system.string contains (string-upcase str) (string-upcase sub)))

  (define/contract (string-starts-with? str:string sub:string)
    (clr-call system.string startswith str sub))

  (define/contract (string-ci-starts-with? str:string sub:string)
    (clr-call system.string startswith (string-upcase str) (string-upcase sub)))

  (define/contract (string-ends-with? str:string sub:string)
    (clr-call system.string endswith str sub))

  (define/contract (string-ci-ends-with? str:string sub:string)
    (clr-call system.string endswith (string-upcase str) (string-upcase sub)))

  (define/contract (string-trim str:string)
    (clr-call System.String trim str))

          
)
  