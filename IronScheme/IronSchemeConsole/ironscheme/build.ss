(library (ironscheme build)
  (export ironscheme-build)
  (import 
    (rnrs)
    (only (ironscheme interaction) load))
  
  (define (ironscheme-build)
    (load "ironscheme-buildscript.ss"))
)