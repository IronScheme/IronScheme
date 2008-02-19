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
    load
    compile)
  (import 
    (rnrs base)
    (rnrs control)
    (rnrs io simple)
    (rnrs programs)
    (psyntax compat)
    (psyntax internal)
    (psyntax library-manager)
    (psyntax expander))
    
  (define (load filename)
    (load-r6rs-top-level filename 'load)
    (void))
    
  (define (compile filename)
    (load-r6rs-top-level filename 'compile))
  
  (define (load-r6rs-top-level filename how)
    (let ((x* 
           (with-input-from-file filename
             (lambda ()
               (let f ()
                 (let ((x (read-annotated)))
                   (if (eof-object? x) 
                       '()
                       (cons x (f)))))))))
      (case how
        ((load)      ((compile-r6rs-top-level x*)))
        ((compile)   
            (begin 
					    (compile-r6rs-top-level x*) ; i assume this is needed
					    (serialize-all serialize-library 
					      (lambda (x) 
					        (compile-core-expr (expanded->core x)))))))))

  (let ((args (command-line)))
    (unless (= (length args) 2)
      (display "provide a script name argument\n")
      (exit 17))
    (let ((script-name (car args)) (args (cdr args)))
      (load (car args))))
      
  (current-precompiled-library-loader load-serialized-library)      
  ;; return 'hook', we are cheap :)
  eval-top-level
  )

