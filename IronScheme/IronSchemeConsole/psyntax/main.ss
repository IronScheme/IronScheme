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
    (rename (load-r6rs-top-level load)))
  (import 
    (rnrs base)
    (rnrs control)
    (rnrs io simple)
    (rnrs programs)
    (psyntax compat)
    (psyntax library-manager)
    (psyntax expander))
  
  (define (load-r6rs-top-level filename)
    (let ((x* 
           (with-input-from-file filename
             (lambda ()
               (let f ()
                 (let ((x (read-annotated)))
                   (if (eof-object? x) 
                       '()
                       (cons x (f)))))))))
      (eval-r6rs-top-level x*)
      (if #f #f)))

  (let ((args (command-line)))
    (unless (= (length args) 2)
      (display "provide a script name argument\n")
      (exit 17))
    (let ((script-name (car args)) (args (cdr args)))
      (load-r6rs-top-level (car args))))
  ;; return 'hook'
  eval-top-level
  )

