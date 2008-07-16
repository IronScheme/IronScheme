;;; Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

(library (psyntax main)
  (export
    trace-printer
    command-line
    load
    load/args
    ironscheme-build
    compile
    compile-system-libraries
    compile->closure)
  (import 
    (rnrs base)
    (rnrs control)
    (rnrs io simple)
    (rnrs lists)
    (only (rnrs conditions) serious-condition?)
    (only (rnrs exceptions) raise)
    (psyntax compat)
    (psyntax internal)
    (psyntax library-manager)
    (psyntax expander)
    (only (ironscheme core) get-command-line format)
    (ironscheme files)
    (ironscheme library))
    
  (define trace-printer (make-parameter write))
  
  (define command-line (make-parameter (get-command-line)))  
    
  (define (local-library-path filename)
    (cons (get-directory-name filename) (library-path)))

  (define (load/args filename . args)
    (apply load-r6rs-top-level filename 'load args)
    (void))

  (define (load filename)
    (apply load-r6rs-top-level filename 'load (command-line))
    (void))
      
  (define (ironscheme-build)
    (load "ironscheme-buildscript.ss")) 
    
  (define (eval-top-level x)
    (eval x (interaction-environment)))    
    
  (define (compile-system-libraries)
    (eval-top-level 
      `(begin
         (include "system-libraries.ss")
         (compile "system-libraries.ss"))))
    
  (define (compile filename)
    (load-r6rs-top-level filename 'compile))
    
  (define (compile->closure filename)
    (load-r6rs-top-level filename 'closure))
  
  (define (load-r6rs-top-level filename how . args)
    (parameterize ([library-path (local-library-path filename)])
      (let ((x* 
             (with-input-from-file filename
               (lambda ()
                 (let f ()
                   (let ((x (read-annotated)))
                     (if (eof-object? x) 
                         '()
                         (cons x (f)))))))))
        (case how
          ((closure)   (pre-compile-r6rs-top-level x*))
          ((load)      
            (parameterize ([command-line (cons filename (map (lambda (x) (format "~a" x)) args))])
              ((compile-r6rs-top-level x*))))
          ((compile)   
              (begin 
					      (compile-r6rs-top-level x*) ; i assume this is needed
					      (serialize-all serialize-library compile-core-expr)))))))

 
  (current-precompiled-library-loader load-serialized-library)
  
  (set-symbol-value! 'default-exception-handler 
    (lambda (ex)
      (cond
        [(serious-condition? ex) (raise ex)]
        [else 
          (display ex)
          (newline)])))
      
  (set-symbol-value! 'load load)
  (set-symbol-value! 'compile compile)
  (set-symbol-value! 'compile->closure compile->closure)
  (set-symbol-value! 'eval-r6rs eval-top-level)
  (set-symbol-value! 'int-env-syms interaction-environment-symbols)
  (set-symbol-value! 'expanded2core expanded->core)
  
  (set-symbol-value! 'trace-printer trace-printer)
  
  (library-path (get-library-paths))
  )

