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


; this is the compile file for libraries
; the utilize this feature, do the following at the console
; > (compile-system-libraries)
; Thats it, you are done :)

(import (ironscheme))

(let () 
  (import 
    (ironscheme clr)
    (ironscheme console)
    (ironscheme conversions)
    (ironscheme datetime)
    (ironscheme environment)
    (ironscheme linq2)
    (ironscheme files)
    (ironscheme process)
    (ironscheme random)
    (ironscheme record-case)
    (ironscheme regex)
    (ironscheme registry)
    (ironscheme strings)
    (ironscheme symbolic-case)
    (ironscheme threading)
    (ironscheme web)
    (ironscheme web-utils)
    (ironscheme web routing-helper)
    (ironscheme web controllers)
    (ironscheme web views)
    (ironscheme xml)
    (ironscheme docs)
    
    (ironscheme clr reflection)
    (ironscheme clr shorthand)
    
    (ironscheme ffi)
    (ironscheme integrable)
    (ironscheme library-utils)

    (ironscheme syntax-format)
    (ironscheme define-macro)
    (ironscheme contracts)

    (ironscheme collections arraylist)
    (ironscheme collections icollection)
    (ironscheme collections ilist)
    (ironscheme collections stack))
  #f)


(let () 
  (include "srfi/compile-all.ironscheme.sps")
  #f)


(let () 
  (import 
    (ikarus)
    (syn-param)
    (foof-loop)
    (list-match)
    (as-match))
  #f)
  
(let () 
  (import 
    (ironscheme linq)
    (match)) ; seperate
  #f)
 