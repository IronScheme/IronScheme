(library (ironscheme files)
  (export
    file-exists?
    delete-file
    file-copy
    file-move
    
    directory-exists?
    delete-directory
    directory-move
    create-directory
    get-files
    get-directories
    )
    
  (import 
    (except (rnrs) file-exists? delete-file)
    (ironscheme clr))
    
  (clr-using system.io)
    
  (define (file-exists? fn)
    (clr-static-call file exists fn))
    
  (define (delete-file fn)
    (clr-static-call file delete fn))
    
  (define file-copy
    (case-lambda
      [(from to)            (file-copy from to #f)]
      [(from to overwrite?) (clr-static-call file copy from to overwrite?)]))
       
  (define (file-move from to)
    (clr-static-call file move from to))    
    
  (define (directory-exists? fn)
    (clr-static-call directory exists fn))
    
  (define delete-directory 
    (case-lambda
      [(dir)            (delete-directory dir #f)]
      [(dir recursive?) (clr-static-call directory delete dir recursive?)]))
    
  (define (directory-move from to)
    (clr-static-call directory move from to))    
    
  (define (create-directory name)
    (clr-static-call directory createdirectory name))      
    
  (define get-files 
    (case-lambda
      [(dir)            (get-files dir "*")]
      [(dir pattern)    (clr-static-call directory getfiles dir pattern)]))   

  (define get-directories 
    (case-lambda
      [(dir)            (get-directories dir "*")]
      [(dir pattern)    (clr-static-call directory getdirectories dir pattern)]))    
    
  ;; todo: path procs   
    
  (clr-clear-usings)
    
)