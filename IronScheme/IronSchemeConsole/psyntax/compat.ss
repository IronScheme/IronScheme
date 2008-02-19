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
  (export make-parameter parameterize define-record pretty-print
          gensym void eval-core symbol-value set-symbol-value! file-options-spec
          read-annotated annotation? annotation-expression annotation-source
          compile-core-expr load-serialized-library serialize-library
          annotation-stripped make-record-printer load-precompiled-library)
  (import 
    (rnrs)
    (ironscheme reader)
    (ironscheme records printer)
    (ironscheme serialization)
    (only (psyntax system $bootstrap)
          void gensym eval-core set-symbol-value! symbol-value 
          pretty-print))
  
  (define (load-precompiled-library filename sk) #f) ; used?
  
  #|
  (define read-annotated read)
  (define (annotation? x) #f)
  (define annotation-expression #f)
  (define annotation-source #f)
  (define annotation-stripped #f)
  
  (define (compile-core-expr x) #f)
  (define (load-serialized-library filename sk) #f)
  (define (serialize-library filename contents) #f)
  |#

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

  ;;; we represent records as vectors for portability but this is 
  ;;; not nice.  If your system supports compile-time generative
  ;;; records, replace the definition of define-record with your 
  ;;; system supplied definition (which you should support in the 
  ;;; expander first of course).
  ;;; if your system allows associating printers with records, 
  ;;; a printer procedure is provided (so you can use it in the 
  ;;; output of the macro).  The printers provided take two 
  ;;; arguments, a record instance and an output port.  They 
  ;;; output something like #<stx (foo bar)> or #<library (rnrs)> 
  ;;; to the port.
  ;;;
  ;;; The following should be good for full R6RS implementations.
  ;;;
                       
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
#|

  (define-syntax define-record
    (lambda (stx)
      (define (iota i j)
        (cond
          ((= i j) '())
          (else (cons i (iota (+ i 1) j)))))
      (syntax-case stx ()
        ((_ name (field* ...) printer) 
         (syntax (define-record name (field* ...))))
        ((_ name (field* ...))
         (with-syntax ((constructor 
                        (datum->syntax (syntax name)
                          (string->symbol
                            (string-append "make-"
                              (symbol->string 
                                (syntax->datum (syntax name)))))))
                       (predicate 
                        (datum->syntax (syntax name)
                          (string->symbol
                            (string-append 
                              (symbol->string 
                                (syntax->datum (syntax name)))
                              "?")))) 
                       (<rtd> 
                        (datum->syntax (syntax name) (gensym)))
                       ((accessor ...)
                        (map 
                          (lambda (x) 
                            (datum->syntax (syntax name)
                              (string->symbol 
                                (string-append 
                                  (symbol->string (syntax->datum
                                                    (syntax name)))
                                  "-"
                                  (symbol->string (syntax->datum x))))))
                          (syntax (field* ...)))) 
                       ((mutator ...)
                        (map 
                          (lambda (x) 
                            (datum->syntax (syntax name)
                              (string->symbol 
                                (string-append "set-" 
                                  (symbol->string (syntax->datum
                                                    (syntax name)))
                                  "-"
                                  (symbol->string (syntax->datum x))
                                  "!"))))
                          (syntax (field* ...))))
                       ((idx ...)
                        (iota 1 (+ 1 (length (syntax (field* ...)))))))
           (syntax (begin
               (define constructor
                 (lambda (field* ...) 
                   (vector '<rtd> field* ...)))
               (define predicate
                 (lambda (x) 
                   (and (vector? x) 
                        (= (vector-length x) 
                           (+ 1 (length '(field* ...))))
                        (eq? (vector-ref x 0) '<rtd>))))
               (define accessor
                 (lambda (x)
                   (if (predicate x) 
                       (vector-ref x idx)
                       (error 'accessor "~s is not of type ~s" x
                              'name))))
               ...
               (define mutator
                 (lambda (x v)
                   (if (predicate x) 
                       (vector-set! x idx v)
                       (error 'mutator "~s is not of type ~s" x
                              'name))))
               ...)))))))
|#    

  (define (file-options-spec x) 
    (error 'file-options-spec "not implemented"))

)





