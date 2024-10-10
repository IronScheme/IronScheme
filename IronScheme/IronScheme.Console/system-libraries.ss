#| License
Copyright (c) 2007-2016 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

; this is the compile file for libraries
; the utilize this feature, do the following at the console
; > (compile-system-libraries)
; Thats it, you are done :)

(import (ironscheme))

(include "system-libraries.ironscheme.ss")

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
  
