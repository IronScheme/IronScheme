#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme web models)
  (export
    save-data
    load-data)
  (import
    (ironscheme)
    (ironscheme threading)
    (ironscheme web))

  (define (save-data filename data)
    (lock (filename)  
      (let ((fn (map-path filename)))
        (delete-file fn)
        (call-with-port (open-file-output-port fn)
          (lambda (p)
            (serialize-port data p))))))
    
  (define (load-data filename)
    (lock (filename)  
      (let ((fn (map-path filename)))
        (if (file-exists? fn)
            (call-with-port (open-file-input-port fn)
              (lambda (p)
                (guard (e (#t (begin (delete-file fn) #f)))
                  (deserialize-port p))))
            #f)))))