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


(define (split ls)
  (cond
    ((null? ls) (values '() '()))
    (else 
     (call-with-values (lambda () (split (cdr ls)))
       (lambda (cars cdrs)
         (let ((a (car ls)))
           (values (cons (car a) cars)
                   (cons (cdr a) cdrs))))))))

(define for-all ;;; almost
  (lambda (f . args)
    (if (all-empty? args) 
        #t
        (call-with-values (lambda () (split args))
          (lambda (cars cdrs)
            (and (apply f cars) 
                 (apply for-all f cdrs)))))))

(define exists  ;;; almost
  (lambda (f . args)
    (if (all-empty? args) 
        #f
        (call-with-values (lambda () (split args))
          (lambda (cars cdrs)
            (or (apply f cars)
                (apply exists f cdrs)))))))

(define (open-string-output-port)
  (let ((p (open-output-string)))
    (values p (lambda () (get-output-string p))))) 

(define command-line 
  (lambda () 
    (list "ironscheme" r6rs-input))) ; defined in init.scm
    
(load "psyntax.pp")
