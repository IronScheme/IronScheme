#| License
Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#


; this is the compile file for libraries
; the utilize this feature, do the following at the console
; > (compile-system-libraries)
; Thats it, you are done :)

(import (ironscheme))

(let () 
  (import 
    (ironscheme clr)
    
    (ironscheme io)
    (ironscheme fsm-cond)
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
    (ironscheme regex-cond)
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
    (ironscheme clr dynamic)
    
    ;(ironscheme ffi) ; cant compile due to dynamic codegen
    (ironscheme integrable)
    (ironscheme library-utils)

    (ironscheme syntax-format)
    (ironscheme define-macro)
    (ironscheme contracts)
    
    (ironscheme debugger)

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
  
