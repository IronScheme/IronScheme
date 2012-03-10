;;;; fmt-js.scm -- javascript formatting utilities
;;
;; Copyright (c) 2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (js-expr x)
  (lambda (st) (((or (fmt-gen st) js-expr/sexp) x) st)))

(define (js-expr/sexp x)
  (cond
   ((procedure? x)
    x)
   ((pair? x)
    (case (car x)
      ((%fun function) (apply js-function (cdr x)))
      ((%var var) (js-var x))
      ((eq? ===) (apply js=== (cdr x)))
      ((>>>) (apply js>>> (cdr x)))
      ((%array) (js-array x))
      ((%object) (js-object (cdr x)))
      ((%comment) (js-comment x))
      (else (c-expr/sexp x))))
   ((vector? x) (js-array x))
   ((boolean? x) (cat (if x "true" "false")))
   ((char? x) (js-expr/sexp (string x)))
   (else (c-expr/sexp x))))

(define (js-function . x)
  (let* ((name (and (symbol? (car x)) (car x)))
         (params (if name (cadr x) (car x)))
         (body (if name (cddr x) (cdr x))))
    (c-block
     (cat "function " (dsp (or name ""))  "("
          (fmt-join dsp params ", ") ")")
     (fmt-let 'return? #t (c-in-stmt (apply c-begin body))))))

(define (js-var x)
  (apply c-var 'var x))

(define (js=== . args)
  (apply c-op "===" args))

(define (js>>> . args)
  (apply c-op ">>>" args))

(define (js-comment . args)
  (columnar "// " (apply-cat args)))

(define (js-array x)
  (let ((ls (vector->list x)))
    (c-wrap-stmt
     (fmt-try-fit
      (fmt-let 'no-wrap? #t (cat "[" (fmt-join js-expr ls ", ") "]"))
      (lambda (st)
        (let* ((col (fmt-col st))
               (sep (string-append "," (make-nl-space col))))
          ((cat "[" (fmt-join js-expr ls sep) "]" nl) st)))))))

(define (js-pair x)
  (cat (js-expr (car x)) ": " (js-expr (cdr x))))

(define (js-object ls)
  (c-in-expr
   (fmt-try-fit
    (fmt-let 'no-wrap? #t (cat "{" (fmt-join js-pair ls ", ") "}"))
    (lambda (st)
      (let* ((col (fmt-col st))
             (sep (string-append "," (make-nl-space col))))
        ((cat "{" (fmt-join js-pair ls sep) "}" nl) st))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
