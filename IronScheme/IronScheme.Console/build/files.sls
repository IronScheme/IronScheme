#| License
Copyright (c) 2007-2016 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme files)
  (export
    file-exists?
    delete-file
    get-directory-name
    file-newer?
    file-mtime)
    
  (import 
    (except (rnrs) file-exists? delete-file)
    (ironscheme contracts)
    (ironscheme typed)
    (ironscheme clr))
    
  (clr-using System.IO)
  (clr-using Oyster.Math)
  (clr-using IronScheme.Scripting.Math)

  (define: (->string str -> string)
    (if (clr-is String str)
        str
        (clr-call Object ToString str)))

  (define: (file-exists? fn -> bool)
    (unless (string? fn)
      (assertion-violation 'file-exists? "not a string" fn))
    (clr-static-call File Exists (->string fn)))
    
  (define/contract (delete-file fn:string)
    (clr-static-call File Delete (->string fn)))
    
  (define: (get-directory-name path -> string)
    (unless (string? path)
      (assertion-violation 'get-directory-name "not a string" path))
    (clr-static-call Path GetDirectoryName (->string path)))   
    
  (define: (get-last-write-time filename -> DateTime)
    (unless (string? filename)
      (assertion-violation 'get-last-write-time "not a string" filename))
    (clr-static-call File GetLastWriteTime (->string filename)))
    
  (define: (file-mtime filename -> IntX)
    (let ((dt (get-last-write-time filename)))
      (clr-static-call IntX (Create Int64) (clr-prop-get DateTime Ticks dt))))
    
  (define: (compare-time (t1 : DateTime)(t2 : DateTime) -> fixnum)
    (clr-call DateTime CompareTo t1 t2))    
    
  (define: (file-newer? file1 file2 -> bool)
    (let ((r (compare-time (get-last-write-time file1)
                           (get-last-write-time file2))))
      (fx>=? r 0))))