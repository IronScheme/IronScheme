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

(library (psyntax compat)
  (export make-parameter parameterize define-record compile-core
          gensym void eval-core symbol-value set-symbol-value! file-options-spec
          read-annotated annotation? annotation-expression annotation-source
          load-serialized-library serialize-library
          annotation-stripped make-record-printer read-library-source-file)
  (import 
    (rnrs)
    (ironscheme reader)
    (ironscheme records printer)
    (ironscheme serialization)
    (only (psyntax system $bootstrap)
          void gensym eval-core set-symbol-value! symbol-value compile-core))

  (define (read-library-source-file file-name)
		(with-input-from-file file-name read-annotated))
  
  (define make-parameter
    (case-lambda
      ((x) (make-parameter x (lambda (x) x)))
      ((x fender)
       (assert (procedure? fender))
       (let ((x (fender x)))
         (case-lambda
           (() x)
           ((v) (set! x (fender v))))))))

  (define-syntax parameterize 
    (lambda (x)
      (syntax-case x ()
        ((_ () b b* ...) (syntax (let () b b* ...)))
        ((_ ((olhs* orhs*) ...) b b* ...)
         (with-syntax (((lhs* ...) (generate-temporaries (syntax (olhs* ...))))
                       ((rhs* ...) (generate-temporaries (syntax (olhs* ...)))))
           (syntax (let ((lhs* olhs*) ...
                   (rhs* orhs*) ...)
               (let ((swap 
                      (lambda () 
                        (let ((t (lhs*)))
                          (lhs* rhs*)
                          (set! rhs* t))
                        ...)))
                 (dynamic-wind 
                   swap
                   (lambda () b b* ...)
                   swap)))))))))
                       
(define-syntax define-record
  (lambda (x)
	  (define (syn->str s)
		  (symbol->string
			  (syntax->datum s)))
    (define (gen-getter id)
      (lambda (fld)
        (datum->syntax id
          (string->symbol
            (string-append (syn->str id) "-" (syn->str fld))))))
    (define (gen-setter id)
      (lambda (fld)
        (datum->syntax id
          (string->symbol
            (string-append "set-" (syn->str id) "-" (syn->str fld) "!")))))
    (syntax-case x ()
      [(_ name (field* ...) printer)
       #`(begin 
           (define-record name (field* ...)) 
           (define rp (make-record-printer 'name printer)))]
      [(_ name (field* ...))
       (with-syntax ([(getter* ...)
                      (map (gen-getter #'name) #'(field* ...))]
                     [(setter* ...)
                      (map (gen-setter #'name) #'(field* ...))])
         #`(define-record-type name
             (sealed #t) ; for better performance
             (opaque #t) ; for security
             (nongenerative) ; for sanity
             (fields (mutable field* getter* setter*) ...)))])))                       

  (define (file-options-spec x) x)
)





