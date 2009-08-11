#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

(library (ironscheme files)
  (export
    file-exists?
    delete-file
    get-directory-name
    file-newer?
    )
    
  (import 
    (except (rnrs) file-exists? delete-file)
    (ironscheme contracts)
    (ironscheme clr))
    
  (clr-using system.io)
    
  (define/contract (file-exists? fn:string)
    (clr-static-call file exists fn))
    
  (define/contract (delete-file fn:string)
    (clr-static-call file delete fn))
    
  (define/contract (get-directory-name path)
    (clr-static-call path getdirectoryname path))   
    
  (define (get-last-write-time filename)
    (clr-static-call File GetLastWriteTime filename))
    
  (define (compare-time t1 t2)
    (clr-call IComparable CompareTo t1 t2))    
    
  (define/contract (file-newer? file1:string file2:string)
    (let ((r (compare-time (get-last-write-time file1)
                           (get-last-write-time file2))))
      (>= r 0)))
)