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

(define cons* 
  (lambda (a . rest) 
    (let f ((a a) (rest rest))
      (if (null? rest) 
          a
          (cons a (f (car rest) (cdr rest))))))) 

(define (eval-core x) (eval x))

'(define char<=?
  (let ((char<=? char<=?))
    (lambda args
      (or (null? args) 
          (let f ((a (car args)) (d (cdr args)))
            (or (null? d) 
                (let ((b (car d)) (d (cdr d)))
                  (and (char<=? a b) (f b d)))))))))
                  
(define (all-empty? ls)
  (or (null? ls) 
      (and (null? (car ls)) 
           (all-empty? (cdr ls)))))

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

(define make-eq-hashtable 
  (lambda () (cons '() #f)))

(define hashtable-ref 
  (lambda (h x v) 
    (cond
      ((assq x (car h)) => cdr)
      (else v))))

(define hashtable-set! 
  (lambda (h x v) 
    (cond
      ((assq x (car h)) => (lambda (p) (set-cdr! p v)))
      (else (set-car! h (cons (cons x v) (car h)))))))

(define (open-string-output-port)
  (let ((p (open-output-string)))
    (values p (lambda () (get-output-string p))))) 

(define command-line 
  (lambda () 
    '("ironscheme" "hello-world.ss")))
    
(define (exit reason) (list 'exit reason))

(load "psyntax.pp")
