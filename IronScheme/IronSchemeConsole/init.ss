; this is the init file for the console
; this file is loaded from the current directory
#|
(import 
  (ironscheme)
  (ironscheme linq))
;; this 'pre-compiles' the libraries used in this file  
(compile "init.ss")
|#

