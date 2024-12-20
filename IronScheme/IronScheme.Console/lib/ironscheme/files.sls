#| License
Copyright (c) 2007-2016 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

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
    
    invalid-filename-characters
    invalid-path-characters)    
  (import 
    (rnrs)
    (ironscheme contracts)
    (ironscheme clr))
    
  (clr-using System.IO)
    
  (define/contract file-copy
    (case-lambda
      [(from to)            
        (file-copy from to #f)]
      [(from:string to:string overwrite?:boolean) 
        (clr-static-call File Copy from to overwrite?)]))
       
  (define/contract (file-move from:string to:string)
    (clr-static-call File Move from to))    

  (define/contract (file-create-time fn:string)
    (clr-static-call File (GetCreationTime String) fn))    

  (define/contract (file-access-time fn:string)
    (clr-static-call File (GetLastAccessTime String) fn))    

  (define/contract (file-modified-time fn:string)
    (clr-static-call File (GetLastWriteTime String) fn))    
    
  (define/contract (directory-exists? fn:string)
    (clr-static-call Directory Exists fn))
    
  (define/contract delete-directory 
    (case-lambda
      [(dir)            
        (delete-directory dir #f)]
      [(dir:string recursive?:boolean) 
        (clr-static-call Directory Delete dir recursive?)]))
    
  (define/contract (directory-move from:string to:string)
    (clr-static-call Directory Move from to))    
    
  (define/contract (create-directory name:string)
    (clr-static-call Directory CreateDirectory name))      
    
  (define/contract get-files 
    (case-lambda
      [(dir)            
        (get-files dir "*")]
      [(dir:string pattern:string)    
        (clr-static-call Directory GetFiles dir pattern 'AllDirectories)]))   

  (define/contract get-directories 
    (case-lambda
      [(dir)            
        (get-directories dir "*")]
      [(dir:string pattern:string)    
        (clr-static-call Directory GetDirectories dir pattern 'AllDirectories)]))    
    
  (define/contract (change-extension path:string extension:string)
    (clr-static-call Path ChangeExtension path extension))
    
  (define/contract (path-combine path1:string path2:string)
    (clr-static-call Path (Combine String String) path1 path2))

  (define/contract (get-directory-name path:string)
    (clr-static-call Path (GetDirectoryName String) path))

  (define/contract (get-extension path:string)
    (clr-static-call Path (GetExtension String) path))

  (define/contract (get-filename path:string)
    (clr-static-call Path (GetFileName String) path))

  (define/contract (get-filename-without-extension path:string)
    (clr-static-call Path (GetFileNameWithoutExtension String) path))

  (define/contract (get-full-path path:string)
    (clr-static-call Path (GetFullPath String) path))

  (define/contract (get-path-root path:string)
    (clr-static-call Path (GetPathRoot String) path))

  (define (get-random-filename)
    (clr-static-call Path GetRandomFileName))

  (define (get-temp-filename)
    (clr-static-call Path GetTempFileName))
    
  (define (get-temp-path)
    (clr-static-call Path GetTempPath))    
    
  (define/contract (path-rooted? path:string)
    (clr-static-call Path (IsPathRooted String) path))    

  (define/contract (path-has-extension? path:string)
    (clr-static-call Path (HasExtension String) path))  
    
  (define (invalid-filename-characters)
    (string->list
      (clr-new String (clr-static-call Path GetInvalidFileNameChars))))

  (define (invalid-path-characters)
    (string->list
      (clr-new String (clr-static-call Path GetInvalidPathChars)))))