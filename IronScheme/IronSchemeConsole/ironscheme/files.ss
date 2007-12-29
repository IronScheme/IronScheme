(library (ironscheme files)
  (export
    file-exists?
    delete-file)
    
  (import 
    (rnrs base)
    (ironscheme clr))
    
  (define (file-exists? fn)
    (clr-call system.io.file exists '() fn))
    
  (define (delete-file fn)
    (clr-call system.io.file delete '() fn))
    
)