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
    build-typed-case-lambda build-typed-lambda
    build-case-lambda build-let build-primref build-foreign-call
    build-data build-sequence build-void build-letrec build-letrec*
    build-global-define build-library-letrec*)
  (import (rnrs) (psyntax compat)
          (only (ironscheme) debug-mode? lw-debug-mode?))
  
  (define (build-global-define x)
    (build-void))
  (define build-application
    (lambda (ae fun-exp arg-exps)
      (if (and (or (debug-mode?) (lw-debug-mode?)) ae)
          `(annotated-call ,ae ,fun-exp . ,arg-exps)
          (cons fun-exp arg-exps))))
  (define-syntax build-conditional
    (syntax-rules ()
      ((_ ae test-exp then-exp else-exp)
       `(if ,test-exp ,then-exp ,else-exp))))
  (define build-lexical-reference
    (lambda (ae var)
      (if (and (or (debug-mode?) (lw-debug-mode?)) ae)
          `(annotated-call ,ae . ,var)
          var)))
  (define build-lexical-assignment
    (lambda (ae var exp)
      (if (and (or (debug-mode?) (lw-debug-mode?)) ae)
          `(annotated-call ,ae set! ,var ,exp)
          `(set! ,var ,exp))))
  (define build-global-reference
    (lambda (ae var)
      (if (and (or (debug-mode?) (lw-debug-mode?)) ae)
          `(annotated-call ,ae . ,var)
          var)))
  (define build-global-assignment
    (lambda (ae var exp)
      (if (and (or (debug-mode?) (lw-debug-mode?)) ae)
          `(annotated-call ,ae set! ,var ,exp)
          `(set! ,var ,exp))))
  (define-syntax build-global-definition
    (syntax-rules ()
      ((_ ae var exp) (build-global-assignment ae var exp))))
  (define build-lambda
    (lambda (ae vars exp) 
      (build-case-lambda ae (list vars) (list exp))))
  (define build-typed-lambda
    (lambda (ae vars type-spec exp) 
      `(typed-case-lambda [,vars ,type-spec ,exp])))        
  (define build-typed-case-lambda
    (lambda (ae vars* type-spec* exp*)
      `(typed-case-lambda . ,(map list vars* type-spec* exp*))))
  (define build-case-lambda
    (lambda (ae vars* exp*)
      (if (and (or (debug-mode?) (lw-debug-mode?)) ae)
          `(annotated-case-lambda ,ae . ,(map list vars* exp*))
          `(case-lambda . ,(map list vars* exp*)))))
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
  (define build-data
    (lambda (ae exp)
      (if (and (or (debug-mode?) (lw-debug-mode?)) ae)
          `(annotated-call ,ae . ',exp)
          `',exp)))
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
      (if (null? vars) 
          body-exp 
          `(letrec ,(map list vars val-exps) ,body-exp))))
  (define build-letrec*
    (lambda (ae vars val-exps body-exp)
      (cond
        ((null? vars) body-exp)
        (else
          `(letrec* ,(map list vars val-exps) ,body-exp)))))
  (define build-library-letrec*
    (lambda (ae name vars locs val-exps body-exp)
      `(library-letrec* ,name ,(map list vars locs val-exps) ,body-exp))))