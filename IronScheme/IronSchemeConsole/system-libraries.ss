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
    (ironscheme linq)
    (ironscheme files)
    (ironscheme pretty-print)
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

    (ironscheme syntax-format)
    (ironscheme define-macro)
    (ironscheme typed)

    (ironscheme collections arraylist)
    (ironscheme collections icollection)
    (ironscheme collections ilist)
    (ironscheme collections stack))
  #f)

(let () 
  (import 
    (srfi lists)
    (srfi streams)
    (srfi land)
    (srfi and-let)
    (srfi string-ports)
    (srfi receive)
    (srfi parameters)
    (srfi format))
  #f)




