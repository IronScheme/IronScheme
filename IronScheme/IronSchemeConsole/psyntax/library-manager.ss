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

(library (psyntax library-manager)
  (export imported-label->binding library-subst installed-libraries
    visit-library library-name library-version library-exists?
    find-library-by-name install-library library-spec invoke-library 
    current-library-expander
    current-library-collection library-path library-extensions
    serialize-all current-precompiled-library-loader)
  (import (except (rnrs) library) (psyntax compat) (rnrs r5rs) (only (ironscheme) format fprintf))

  (define (make-collection)
    (let ((set '()))
      (define (set-cons x ls)
        (cond
          ((memq x ls) ls)
          (else (cons x ls))))
      (case-lambda
        (() set)
        ((x) (set! set (set-cons x set))))))

  (define current-library-collection
    ;;; this works now because make-collection is a lambda
    ;;; binding and this turns into a complex binding as far
    ;;; as letrec is concerned.  It will be more ok once we do
    ;;; letrec*.
    (make-parameter (make-collection)
      (lambda (x)
        (unless (procedure? x)
          (assertion-violation 'current-library-collection "not a procedure" x))
        x)))

  (define-record library 
    (id name version imp* vis* inv* subst env visit-state
        invoke-state visit-code invoke-code visible?
        source-file-name)
    (lambda (x p)
      (unless (library? x)
        (assertion-violation 'record-type-printer "not a library"))
      (display 
        (format "#<library ~s>" 
          (if (null? (library-version x))
              (library-name x)
              (append (library-name x) (list (library-version x)))))
        p)))

  (define (find-dependencies ls)
    (cond
      ((null? ls) '())
      (else (assertion-violation 'find-dependencies "cannot handle deps yet"))))

  (define (find-library-by pred)
    (let f ((ls ((current-library-collection))))
      (cond
        ((null? ls) #f)
        ((pred (car ls)) (car ls))
        (else (f (cdr ls))))))

  (define library-path
    (make-parameter
      '(".")
      (lambda (x)
        (if (and (list? x) (for-all string? x))
            (map (lambda (x) x) x)
            (assertion-violation 'library-path "not a list of strings" x)))))
  
  (define library-extensions
    (make-parameter
      '(".sls" ".ss" ".scm")
      (lambda (x)
        (if (and (list? x) (for-all string? x))
            (map (lambda (x) x) x)
            (assertion-violation 'library-extensions
              "not a list of strings" x)))))

  (define (library-name->file-name x)
    (let-values (((p extract) (open-string-output-port)))
      (define (display-hex n)
        (cond
          ((<= 0 n 9) (display n p))
          (else (display 
                  (integer->char 
                    (+ (char->integer #\A)
                       (- n 10)))
                  p))))
      (let f ((ls x))
        (unless (null? ls)
          (display "/" p)
          (for-each
            (lambda (c)
              (cond
                ((or (char<=? #\a c #\z)
                     (char<=? #\A c #\Z)
                     (char<=? #\0 c #\9)
                     (memv c '(#\- #\. #\_ #\~)))
                 (display c p))
                (else
                 (display "%" p)
                 (let ((n (char->integer c)))
                   (display-hex (quotient n 16))
                   (display-hex (remainder n 16))))))
            (string->list 
              (symbol->string (car ls))))
          (f (cdr ls))))
      (extract)))

  (define file-locator
    (make-parameter
      (lambda (x)
        (let ((str (library-name->file-name x)))
          (let f ((ls (library-path)) 
                  (exts (library-extensions))
                  (failed-list '()))
            (cond
              ((null? ls)
               (let ()
                 (define-condition-type &library-resolution &condition
                    make-library-resolution-condition
                    library-resolution-condition?
                    (library condition-library)
                    (files condition-files))
                 (raise 
                   (condition 
                     (make-error)
                     (make-who-condition 'expander)
                     (make-message-condition
                       "cannot locate library in library-path")
                     (make-library-resolution-condition 
                       x (reverse failed-list))))))
              ((null? exts) 
               (f (cdr ls) (library-extensions) failed-list))
              (else
               (let ((name (string-append (car ls) str (car exts))))
                 (if (file-exists? name)
                     name
                     (f ls (cdr exts) (cons name failed-list)))))))))
      (lambda (f)
        (if (procedure? f)
            f
            (assertion-violation 'file-locator "not a procedure" f)))))

  (define (serialize-all serialize compile)
    (define (library-desc x) 
      (list (library-id x) (library-name x)))
    (for-each 
      (lambda (x)
        (when (library-source-file-name x) 
          (serialize 
            (library-source-file-name x)
            (list (library-id x) 
                  (library-name x)
                  (library-version x) 
                  (map library-desc (library-imp* x))
                  (map library-desc (library-vis* x))
                  (map library-desc (library-inv* x))
                  (library-subst x)
                  (library-env x)
                  (compile (library-visit-code x))
                  (compile (library-invoke-code x))
                  (library-visible? x)))))
      ((current-library-collection))))

  (define current-precompiled-library-loader
    (make-parameter (lambda (filename sk) #f)))
        
  (define (try-load-from-file filename)
    ((current-precompiled-library-loader)
      filename
      (case-lambda
        [(id name ver imp* vis* inv* exp-subst exp-env
          visit-proc invoke-proc visible?)
         ;;; make sure all dependencies are met
         ;;; if all is ok, install the library
         ;;; otherwise, return #f so that the
         ;;; library gets recompiled.
         (let f ([deps (append imp* vis* inv*)])
           (cond
             [(null? deps)
              (install-library id name ver imp* vis* inv* 
                exp-subst exp-env visit-proc invoke-proc 
                #f #f visible? #f)
              #t]
             [else
              (let ([d (car deps)]) 
                (let ([label (car d)] [dname (cadr d)]) 
                  (let ([l (find-library-by-name dname)]) 
                    (cond
                      [(and (library? l) (eq? label (library-id l)))
                       (f (cdr deps))]
                      [else 
                       (fprintf (current-error-port)
                          "WARNING: library ~s has an inconsistent dependency \
                           on library ~s; file ~s will be recompiled from \
                           source.\n"
                         name dname filename)
                       #f]))))]))]
        [others #f])))

  (define library-loader
    (make-parameter
      (lambda (x)
        (let ((file-name ((file-locator) x)))
          (cond
            [(not file-name) 
             (assertion-violation #f "cannot file library" x)]
            [(try-load-from-file file-name)]
            [else 
             ((current-library-expander)
              (read-library-source-file file-name)
              file-name
              (lambda (name)
                (unless (equal? name x)
                  (assertion-violation 'import
                    (let-values ([(p e) (open-string-output-port)])
                      (display "expected to find library " p)
                      (write x p)
                      (display " in file " p)
                      (display file-name p)
                      (display ", found " p)
                      (write name p)
                      (display " instead" p)
                      (e))))))])))
      (lambda (f)
        (if (procedure? f)
            f
            (assertion-violation 'library-locator 
                   "not a procedure" f)))))

  (define current-library-expander
    (make-parameter
      (lambda (x)
        (assertion-violation 'library-expander "not initialized"))
      (lambda (f)
        (if (procedure? f)
            f
            (assertion-violation 'library-expander 
                   "not a procedure" f)))))

  (define external-pending-libraries 
    (make-parameter '()))

  (define (find-external-library name)
    (when (member name (external-pending-libraries))
      (assertion-violation #f 
        "circular attempt to import library was detected" name))
    (parameterize ((external-pending-libraries
                    (cons name (external-pending-libraries))))
      ((library-loader) name)
      (or (find-library-by
            (lambda (x) (equal? (library-name x) name)))
          (assertion-violation #f
            "handling external library did not yield the correct library"
             name))))
        
  (define (find-library-by-name name)
    (or (find-library-by
          (lambda (x) (equal? (library-name x) name)))
        (find-external-library name)))

  (define (library-exists? name)
    (and (find-library-by
           (lambda (x) (equal? (library-name x) name)))
         #t))

  (define (find-library-by-spec/die spec)
    (let ((id (car spec)))
      (or (find-library-by
            (lambda (x) (eq? id (library-id x))))
          (assertion-violation #f 
            "cannot find library with required spec" spec))))

  (define label->binding-table (make-eq-hashtable))

  (define (install-library-record lib)
    (let ((exp-env (library-env lib)))
      (for-each 
        (lambda (x) 
          (let ((label (car x)) (binding (cdr x)))
            (let ((binding 
                   (case (car binding)
                     ((global) 
                      (cons 'global (cons lib (cdr binding))))
                     ((global-macro)
                      (cons 'global-macro (cons lib (cdr binding))))
                     ((global-macro!)
                      (cons 'global-macro! (cons lib (cdr binding))))
                     (else binding))))
              (hashtable-set! label->binding-table label binding))))
        exp-env))
    ((current-library-collection) lib))

  (define install-library 
    (case-lambda
      [(id name ver imp* vis* inv* exp-subst exp-env 
        visit-proc invoke-proc visit-code invoke-code 
        visible? source-file-name)
       (let ((imp-lib* (map find-library-by-spec/die imp*))
             (vis-lib* (map find-library-by-spec/die vis*))
             (inv-lib* (map find-library-by-spec/die inv*)))
         (unless (and (symbol? id) (list? name) (list? ver))
           (assertion-violation 'install-library 
             "invalid spec with id/name/ver" id name ver))
         (when (library-exists? name)
           (assertion-violation 'install-library 
             "library is already installed" name))
         (let ((lib (make-library id name ver imp-lib* vis-lib* inv-lib* 
                       exp-subst exp-env visit-proc invoke-proc 
                       visit-code invoke-code visible? source-file-name)))
           (install-library-record lib)))]))

  (define (imported-label->binding lab)
    (hashtable-ref label->binding-table lab #f))

  (define (invoke-library lib)
    (let ((invoke (library-invoke-state lib)))
      (when (procedure? invoke)
        (set-library-invoke-state! lib 
          (lambda () (assertion-violation 'invoke "circularity detected" lib)))
        (for-each invoke-library (library-inv* lib))
        (set-library-invoke-state! lib 
          (lambda () 
            (assertion-violation 'invoke "first invoke did not return" lib)))
        (invoke)
        (set-library-invoke-state! lib #t))))


  (define (visit-library lib)
    (let ((visit (library-visit-state lib)))
      (when (procedure? visit)
        (set-library-visit-state! lib 
          (lambda () (assertion-violation 'visit "circularity detected" lib)))
        (for-each invoke-library (library-vis* lib))
        (set-library-visit-state! lib 
          (lambda () 
            (assertion-violation 'invoke "first visit did not return" lib)))
        (visit)
        (set-library-visit-state! lib #t))))


  (define (invoke-library-by-spec spec)
    (invoke-library (find-library-by-spec/die spec)))

  (define installed-libraries 
    (case-lambda
      ((all?)
       (let f ((ls ((current-library-collection))))
         (cond
           ((null? ls) '())
           ((or all? (library-visible? (car ls)))
            (cons (car ls) (f (cdr ls))))
           (else (f (cdr ls))))))
      (() (installed-libraries #f))))

  (define library-spec       
    (lambda (x) 
      (unless (library? x)
        (assertion-violation 'library-spec "not a library" x))
      (list (library-id x) (library-name x) (library-version x)))) 
  )

