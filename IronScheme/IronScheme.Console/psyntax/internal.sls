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
  (export current-primitive-locations compile-core-expr-to-port compile-core-expr-to-fasl expanded->core compile-core-expr)
  (import 
    (rnrs) 
    (psyntax compat) 
    (only (ironscheme) pretty-print serialize-port valuetype-vector?)
    (only (ironscheme clr) clr-is))
  
  (define current-primitive-locations
    (make-parameter 
      (lambda (x) #f)
      (lambda (p)
        (assert (procedure? p))
        p)))

  ; the output is the input language of IronScheme
  (define (rewrite x)
    (define (f x)
      (cond
        ((pair? x) 
         (case (car x)
           ((quote) x)
           ((case-lambda) 
            (cons 'case-lambda
              (map 
                (lambda (x) 
                  (cons (car x) (map f (cdr x))))
                (cdr x))))
           ((typed-case-lambda) 
            (cons 'typed-case-lambda
              (map 
                (lambda (x) 
                  (cons* (car x) (cadr x) (map f (cddr x))))
                (cdr x))))                
           ((annotated-typed-case-lambda) 
            (cons* 'annotated-typed-case-lambda
              (cadr x)
              (map 
                (lambda (x) 
                  (cons* (car x) (cadr x) (map f (cddr x))))
                (cddr x)))) 
           ((annotated-call) 
            (cons* 'annotated-call
                   (cadr x)
                   (f (cddr x))))
           ((annotated-case-lambda) 
            (cons* 'annotated-case-lambda
                   (cadr x)
                   (map 
                     (lambda (x) 
                       (cons (car x) (map f (cdr x))))
                     (cddr x))))
           ((lambda) ; not sure if this is ever hit
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
           ((define) x) ; this does not look correct, I doubt it is ever hit
           (else 
            (if (list? x) 
                (map f x)
                (error 'rewrite "invalid form ~s ~s" x (list? x))))))
        (else x)))
    (f x))
  
  (define (expanded->core x)
    (rewrite x))
    
  (define (compile-core-expr x)
    (rewrite x))
    
  (define (compile-core-expr-to-fasl x p)
    (serialize-port (rewrite x) p))    

  (define (compile-core-expr-to-port x p)
    (pretty-print (rewrite x) p)))