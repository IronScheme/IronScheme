(library (ironscheme files)
  (export
    file-exists?
    delete-file
    get-directory-name
    file-newer?
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
    
  (define (get-last-write-time filename)
    (clr-static-call File GetLastWriteTime filename))
    
  (define (compare-time t1 t2)
    (clr-call IComparable CompareTo t1 t2))    
    
  (define (file-newer? file1 file2)
    (let ((r (compare-time (get-last-write-time file1)
                           (get-last-write-time file2))))
      (>= r 0)))
)