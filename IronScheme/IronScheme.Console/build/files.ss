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