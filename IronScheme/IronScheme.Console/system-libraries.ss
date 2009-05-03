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
    (ironscheme threading)
    (ironscheme web)
    (ironscheme web-utils)
    (ironscheme web routing-helper)
    (ironscheme web controllers)
    (ironscheme web views)
    (ironscheme xml)
    (ironscheme docs)
    
    (ironscheme clr reflection)
    
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
 