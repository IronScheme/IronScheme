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

(library (psyntax internal)
  (export current-primitive-locations compile-core-expr-to-port expanded->core compile-core-expr)
  (import (rnrs) (psyntax compat) (ironscheme pretty-print))
  
  (define current-primitive-locations
    (make-parameter 
      (lambda (x) #f)
      (lambda (p)
        (assert (procedure? p))
        p)))
  
  (define (mutable? x) 
    (define (simple? x)
      (or (null? x) 
          (char? x) 
          (symbol? x)
          (boolean? x)
          (string? x)
          (bytevector? x)
          (number? x)))
    (not (simple? x)))

  (define (rewriter quote-hack?)
    (define (f x)
      (cond
        ((pair? x) 
         (case (car x)
           ((quote) 
            (cond
              ((and quote-hack? (mutable? (cadr x)))
               (let ((g (gensym)))
                 (set-symbol-value! g (cadr x))
                 g))
              (else x)))
           ((case-lambda) 
            (cons 'case-lambda
              (map 
                (lambda (x) 
                  (cons (car x) (map f (cdr x))))
                (cdr x))))
           ((lambda) 
            (cons* 'lambda (cadr x) (map f (cddr x))))
           ((letrec) 
            (let ((bindings (cadr x)) (body* (cddr x)))
              (let ((lhs* (map car bindings)) (rhs* (map cadr bindings)))
                (cons* 'letrec
                       (map list lhs* (map f rhs*))
                       (map f body*))))) 
           ((letrec*) 
            (let ((bindings (cadr x)) (body* (cddr x)))
              (let ((lhs* (map car bindings)) (rhs* (map cadr bindings)))
                (cons* 'letrec*
                       (map list lhs* (map f rhs*))
                       (map f body*)))))
           ((library-letrec*) 
            (let ((name (cadr x))(x (cdr x)))
              (let ((bindings (cadr x)) (body* (cddr x)))
                (let ((lhs* (map car bindings)) 
                      (lhs** (map cadr bindings)) 
                      (rhs* (map caddr bindings)))
                  (cons* 'library-letrec* name
                         (map list lhs* lhs** (map f rhs*))
                         (map f body*))))))                       
           ((begin) 
            (cons 'begin (map f (cdr x))))
           ((set!) 
            (list 'set! (cadr x) (f (caddr x))))
           ((primitive) 
            (let ((op (cadr x)))
              (cond
                (((current-primitive-locations) op) =>
                 (lambda (loc)
                   loc))
                (else op))))
           ((define) x)
           (else 
            (if (list? x) 
                (map f x)
                (error 'rewrite "invalid form ~s ~s" x (list? x))))))
        (else x)))
    f)
  
  #;(define need-quote-hack?
    (let ((x (cons 1 2)))
      (not (eq? (eval-core `',x) (eval-core `',x)))))

  (define (expanded->core x)
    ((rewriter #t) x))
    
  (define (compile-core-expr x)
    ((rewriter #f) x))    

  (define (compile-core-expr-to-port x p)
    (pretty-print ((rewriter #f) x) p)))


