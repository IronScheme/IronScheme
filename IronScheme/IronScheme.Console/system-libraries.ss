#| License
Copyright (c) 2007-2015 Llewellyn Pritchard
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
    (ironscheme linq)
    (ironscheme files)
    (ironscheme process)
    (ironscheme random)
    (ironscheme record-case)
    (ironscheme regex)
    (ironscheme regex-cond)
    (ironscheme registry)
    
    (ironscheme strings)
    
    (ironscheme threading)
    (ironscheme web)
    (ironscheme web-utils)
    (ironscheme web routing-helper)
    (ironscheme web controllers)
    (ironscheme web views)
    (ironscheme web models)
    (ironscheme xml)
    
    (ironscheme clr reflection)
    (ironscheme clr shorthand)
    (ironscheme clr dynamic)
    
    ;(ironscheme ffi) ; cant compile due to dynamic codegen
    (ironscheme integrable)
    

    (ironscheme syntax)
    (ironscheme syntax define-macro)
    (ironscheme syntax parameters)
    (ironscheme syntax library-utils)
    (ironscheme syntax symbolic-case)
    
    (ironscheme typed)
    (ironscheme typed struct)
    (ironscheme typed struct-case)
    
    (ironscheme contracts)
    
    (ironscheme debugger))
  #f)
  
(let ()
  (import (ironscheme typed language))
  #f)
  
(let ()
  (import (ironscheme typed fixnums))
  #f)
  

(let () 
  (include "system-libraries.srfi.ss")
  #f)
  
(let ()
  (import
    (wak syn-param)
    (wak foof-loop)
    (wak riastreams)
    (wak foof-loop nested)
    (wak fmt)
    (wak fmt js)
    (wak fmt color)
    (wak fmt c)
    (wak trc-testing)
    )
  #f)
      

(let () 
  (import 
    (pfds bbtrees)
    (pfds deques)
    (pfds sets)
    (pfds dlists)
    (pfds psqs)
    (pfds queues)
    (pfds fingertrees)
    (pfds sequences)
    (pfds heaps)
    (pfds hamts))
  #f)

(let () 
  (import 
    (minikanren)
    (list-match)
    (as-match)
    (fectors))
  #f)
  
(let () 
  (import 
    (match)) ; seperate
  #f)
  
