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

(library (psyntax builders)
  (export build-lexical-assignment build-global-reference
    build-application build-conditional build-lexical-reference
    build-global-assignment build-global-definition build-lambda
    build-case-lambda build-let build-primref build-foreign-call
    build-data build-sequence build-void build-letrec build-letrec*
    build-global-define build-library-letrec*)
  (import (rnrs) (psyntax compat) (psyntax config))

  (define (build-global-define x)
    (if-wants-global-defines
      `(define ,x '#f)
      (build-void)))
  (define-syntax build-application
    (syntax-rules ()
      ((_ ae fun-exp arg-exps)
       `(,fun-exp . ,arg-exps))))
  (define-syntax build-conditional
    (syntax-rules ()
      ((_ ae test-exp then-exp else-exp)
       `(if ,test-exp ,then-exp ,else-exp))))
  (define-syntax build-lexical-reference
    (syntax-rules ()
      ((_ ae var) var)))
  (define-syntax build-lexical-assignment
    (syntax-rules ()
      ((_ ae var exp) `(set! ,var ,exp))))
  (define-syntax build-global-reference
    (syntax-rules ()
      ((_ ae var) var)))
  (define-syntax build-global-assignment
    (syntax-rules ()
      ((_ ae var exp) `(set! ,var ,exp))))
  (define-syntax build-global-definition
    (syntax-rules ()
      ((_ ae var exp) (build-global-assignment ae var exp))))
  (define build-lambda
    (lambda (ae vars exp) 
      (if-wants-case-lambda
          `(case-lambda (,vars ,exp))
          `(lambda ,vars ,exp))))
  (define build-case-lambda
    (if-wants-case-lambda
      (lambda (ae vars* exp*)
        `(case-lambda . ,(map list vars* exp*)))
      (lambda (ae vars* exp*)
        (define (build-error ae)
          (build-application ae 
            (build-primref ae 'error) 
            (list (build-data ae 'apply) 
                  (build-data ae "invalid arg count"))))
        (define (build-pred ae n vars) 
          (let-values (((count pred) 
                        (let f ((vars vars) (count 0))
                          (cond
                            ((pair? vars) (f (cdr vars) (+ count 1)))
                            ((null? vars) (values count '=))
                            (else (values count '>=))))))
            (build-application ae (build-primref ae pred) 
              (list (build-lexical-reference ae n) 
                    (build-data ae count)))))
        (define (build-apply ae g vars exp)
          (build-application ae (build-primref ae 'apply) 
            (list (build-lambda ae vars exp) 
                  (build-lexical-reference ae g))))
        (define (expand-case-lambda ae vars exp*) 
          (let ((g (gensym)) (n (gensym)))
            `(lambda ,g
               ,(build-let ae 
                  (list n) (list (build-application ae
                                   (build-primref ae 'length)
                                   (list (build-lexical-reference ae g))))
                  (let f ((vars* vars*) (exp* exp*))
                    (if (null? vars*)
                        (build-error ae)
                        (build-conditional ae
                          (build-pred ae n (car vars*))
                          (build-apply ae g (car vars*) (car exp*))
                          (f (cdr vars*) (cdr exp*)))))))))
        (if (= (length exp*) 1) 
            (build-lambda ae (car vars*) (car exp*))
            (expand-case-lambda ae vars* exp*)))))
  (define build-let
    (lambda (ae lhs* rhs* body)
      (build-application ae (build-lambda ae lhs* body) rhs*)))
  (define-syntax build-primref
    (syntax-rules ()
      ((_ ae name)   (build-primref ae 1 name))
      ((_ ae level name) `(primitive ,name))))
  (define-syntax build-foreign-call
    (syntax-rules ()
      ((_ ae name arg*) `(foreign-call ,name . ,arg*))))
  (define-syntax build-data
    (syntax-rules ()
      ((_ ae exp) `',exp)))
  (define build-sequence
    (lambda (ae exps)
      (let loop ((exps exps))
        (if (null? (cdr exps))
            (car exps)
            (if (equal? (car exps) (build-void))
                (loop (cdr exps))
                `(begin ,@exps))))))
  (define build-void
    (lambda () '((primitive void))))
  (define build-letrec
    (lambda (ae vars val-exps body-exp)
      (if (null? vars) body-exp `(letrec ,(map list vars val-exps) ,body-exp))))
  (define build-letrec*
    (lambda (ae vars val-exps body-exp)
      (cond
        ((null? vars) body-exp)
        (else
         (if-wants-letrec*
          `(letrec* ,(map list vars val-exps) ,body-exp)
          (build-let ae vars (map (lambda (x) (build-data ae #f)) vars)
            (build-sequence ae
              (append
                (map (lambda (lhs rhs) 
                       (build-lexical-assignment ae lhs rhs))
                     vars val-exps)
                (list body-exp)))))))))
  (define build-library-letrec*
    (lambda (ae vars locs val-exps body-exp)
      `(library-letrec* ,(map list vars locs val-exps) ,body-exp)))

  )


