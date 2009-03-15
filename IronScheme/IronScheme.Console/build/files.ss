(library (ironscheme files)
  (export
    file-exists?
    delete-file
    get-directory-name
    )
    
  (import 
    (except (rnrs) file-exists? delete-file)
    (ironscheme clr))
    
  (clr-using system.io)
    
  (define (file-exists? fn)
    (clr-static-call file exists fn))
    
  (define (delete-file fn)
    (clr-static-call file delete fn))
    
  (define (get-directory-name path)
    (clr-static-call path getdirectoryname path))    
)