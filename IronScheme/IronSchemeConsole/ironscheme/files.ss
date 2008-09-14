(library (ironscheme files)
  (export
    file-copy
    file-move
    file-create-time
    file-access-time
    file-modified-time
    
    directory-exists?
    delete-directory
    directory-move
    create-directory
    get-files
    get-directories
    
    change-extension
    path-combine
    get-directory-name
    get-extension
    get-filename
    get-filename-without-extension
    get-full-path
    get-path-root
    get-random-filename
    get-temp-filename
    get-temp-path
    path-rooted?
    path-has-extension?
    )
    
  (import 
    (rnrs)
    (ironscheme clr))
    
  (clr-using system.io)
    
  (define file-copy
    (case-lambda
      [(from to)            (file-copy from to #f)]
      [(from to overwrite?) (clr-static-call file copy from to overwrite?)]))
       
  (define (file-move from to)
    (clr-static-call file move from to))    

  (define (file-create-time fn)
    (clr-static-call file getcreationtime fn))    

  (define (file-access-time fn)
    (clr-static-call file getlastaccesstime fn))    

  (define (file-modified-time fn)
    (clr-static-call file getlastwritetime fn))    
    
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
    
  (define (change-extension path extension)
    (clr-static-call path changeextension path extension))
    
  (define (path-combine path1 path2)
    (clr-static-call path combine path1 path2))

  (define (get-directory-name path)
    (clr-static-call path getdirectoryname path))

  (define (get-extension path)
    (clr-static-call path getextension path))

  (define (get-filename path)
    (clr-static-call path getfilename path))

  (define (get-filename-without-extension path)
    (clr-static-call path getfilenamewithoutextension path))

  (define (get-full-path path)
    (clr-static-call path getfullpath path))

  (define (get-path-root path)
    (clr-static-call path getpathroot path))

  (define (get-random-filename)
    (clr-static-call path getrandomfilename))

  (define (get-temp-filename)
    (clr-static-call path gettempfilename))
    
  (define (get-temp-path)
    (clr-static-call path gettemppath))    
    
  (define (path-rooted? path)
    (clr-static-call path ispathrooted path))    

  (define (path-has-extension? path)
    (clr-static-call path hasextension path))    

    
  (clr-clear-usings)
    
)