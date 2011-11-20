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
  (export make-parameter parameterize define-record compile-core file-options-constructor
          gensym void eval-core symbol-value set-symbol-value! file-options-spec
          read-annotated annotation? annotation-expression annotation-source
          make-annotation
          annotation-stripped
		      read-library-source-file
          library-version-mismatch-warning
          library-stale-warning compiled-library-exists?
          file-locator-resolution-error change-extension
          label-binding set-label-binding! remove-location relative-filename)
  (import 
    (rnrs)
    (ironscheme reader)
    (ironscheme clr)
    (ironscheme records printer)
    (only (ironscheme) fprintf symbol-bound? remove-location)
    (only (psyntax system $bootstrap)
          void gensym eval-core set-symbol-value! symbol-value compile-core))
          
  (define (replace s b a)
    (clr-call String (Replace String String) s b a))
    
  (define (change-extension filename newext)
    (clr-static-call System.IO.Path ChangeExtension filename newext))  
    
  (define (compiled-library-exists? filename)
    (file-exists? (change-extension filename ".dll")))
          
  (define (relative-filename filename)
    (let ((rf (replace 
                filename 
                (clr-static-prop-get IronScheme.Runtime.Builtins ApplicationDirectory)
                "")))
       (replace        
         (string-append 
            "."
            (replace rf "\\" "/"))
         "./"
         "")))

 (define (library-version-mismatch-warning name depname filename)
    (fprintf (current-error-port)
        "WARNING: library ~s has an inconsistent dependency on library ~s; \
         file ~s will be recompiled from source.\n"
       name depname (relative-filename filename)))

  (define (library-stale-warning name filename)
    (fprintf (current-error-port)
       "WARNING: library ~s is stale; file ~s will be recompiled from source.\n"
       name (relative-filename filename)))

  (define (file-locator-resolution-error libname failed-list pending-list)
    (define-condition-type &library-resolution &condition
         make-library-resolution-condition
         library-resolution-condition?
         (library condition-library)
         (files condition-files))
    (define-condition-type &imported-from &condition
       make-imported-from-condition imported-from-condition?
       (importing-library importing-library))
    (raise 
      (apply condition (make-error)
          (make-who-condition 'expander)
          (make-message-condition
            "cannot locate library in library-path")
          (make-library-resolution-condition 
          libname failed-list)
        (map make-imported-from-condition pending-list))))

  (define (read-library-source-file file-name)
		(call-with-input-file file-name read-annotated))
		
  (define (allocate-local-slot)
    (clr-static-call System.Threading.Thread AllocateDataSlot))

  (define (get-local-data slot)
    (clr-static-call System.Threading.Thread GetData slot))

  (define (set-local-data! slot value)
    (clr-static-call System.Threading.Thread SetData slot value))
  
  (define make-parameter
    (case-lambda
      ((x) (make-parameter x values))
      ((x fender)
       (assert (procedure? fender))
       (let ((x (fender x))
             (slot (allocate-local-slot)))
         (set-local-data! slot x)
         (case-lambda
           (()  
            (let ((value (get-local-data slot)))
              (if (null? value)
                  (let ((value x))
                    (set-local-data! slot value)
                    value)
                  value)))
           ((v) 
            (let ((v (fender v)))
              (set! x v)
              (set-local-data! slot v))))))))

  (define-syntax parameterize 
    (lambda (x)
      (syntax-case x ()
        ((_ () b b* ...) (syntax (let () b b* ...)))
        ((_ ((olhs* orhs*) ...) b b* ...)
         (with-syntax (((lhs* ...) (generate-temporaries #'(olhs* ...)))
                       ((rhs* ...) (generate-temporaries #'(olhs* ...))))
           #'(let ((lhs* olhs*) ...
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
                   swap))))))))
                       
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
      [(k name (field* ...) printer)
        (with-syntax ((pname (datum->syntax #'k
                                            (string->symbol (string-append (symbol->string (syntax->datum #'name)) 
                                                                           "?")))))
         #'(begin 
             (define-record name (field* ...)) 
             (define dummy (add-record-printer! pname printer))))]
      [(_ name (field* ...))
       (with-syntax ([(getter* ...)
                      (map (gen-getter #'name) #'(field* ...))]
                     [(setter* ...)
                      (map (gen-setter #'name) #'(field* ...))])
         #'(define-record-type name
             (sealed #t) ; for better performance
             (opaque #t) ; for security
             (nongenerative) ; for sanity
             (fields (mutable field* getter* setter*) ...)))])))                       
  
  (define file-options-constructor (make-parameter #f))
  
  (define (file-options-spec x) ((file-options-constructor) x))
  
  (define (set-label-binding! label binding)
    (set-symbol-value! label binding))
    
  (define (label-binding label)
    (and (symbol-bound? label) (symbol-value label))))

