;;; Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;; Copyright (c) 2008 Llewellyn Pritchard
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

(library (psyntax main)
  (export
    trace-printer
    command-line
    load
    load-port
    load/args
    load/unload
    ironscheme-build
    ironscheme-test
    emacs-mode?
    display-stacktrace
    compile
    compile-system-libraries
    serializer-port
    open-package
    package-filename
    compile->closure)
  (import 
    (rnrs base)
    (rnrs exceptions)
    (rnrs control)
    (rnrs io simple)
    (rnrs io ports)
    (rnrs lists)
    (rnrs hashtables)
    (rnrs bytevectors)
    (only (rnrs conditions) serious-condition? condition?)
    (only (rnrs exceptions) raise raise-continuable with-exception-handler)
    (psyntax compat)
    (psyntax config)
    (psyntax internal)
    (psyntax library-manager)
    (psyntax expander)
    (only (ironscheme core) get-command-line format)
    (ironscheme enums)
    (ironscheme files)
    (ironscheme cps)
    (ironscheme clr)
    (only (ironscheme unsafe) $fx+ $fx-)
    (ironscheme constant-fold)
    (except (ironscheme library) file-locator)
    (only (ironscheme) printf pretty-print initialize-default-printers debug-mode? serialize-port deserialize-port))
    
  (define trace-printer (make-parameter pretty-print))
      
  (define display-stacktrace (make-parameter #f))
  
  (define command-line (make-parameter (get-command-line))) 
   
  (define emacs-mode? (make-parameter #f))
  
  (define (init-open-package fn)
    (cond
      [(not fn) #f]
      [(procedure? fn)
        (let* ((p (fn))
               (pkg (deserialize-port p))
               (os (port-position p)))
          (close-input-port p)
          (package-map pkg)
          (let ((fl (file-locator)))
            (file-locator 
              (lambda (x)
                (let ((fn (format "~a" x)))
                  (or (and (hashtable-contains? pkg fn) fn)
                      (fl x))))))
          (vector-for-each 
            (lambda (k)
              (hashtable-update! pkg k 
                (lambda (v) 
                  (cons ($fx+ (car v) os)
                        ($fx+ (cdr v) os)))
                #f))
            (hashtable-keys pkg))
          fn)]
      [else #f]))
  
  (define serializer-port (make-parameter #f))
  (define library-map (make-parameter #f))
  
  (define open-package (make-parameter #f init-open-package))
  
  (define package-filename 
    (make-parameter 
      #f 
      (lambda (x)
        (and x 
             (string? x) 
             (file-exists? x)
             (open-package 
               (lambda ()
                 ;(printf "~a\n" x)
                 (open-file-input-port x)))
             x))))
                 
  (define package-map (make-parameter #f))
    
  (define (local-library-path filename)
    (cons (get-directory-name filename) (library-path)))

  (define (load/args filename . args)
    (apply load-r6rs-top-level filename 'load args)
    (void))
    
  (define (load/unload filename)
    (let ((libs #f)) 
      (dynamic-wind
        (lambda ()
          (set! libs (installed-libraries)))
        (lambda ()
          (load filename))
        (lambda ()          
          (for-each
            (lambda (lib)
              (unless (memq lib libs)
                (uninstall-library lib)))
            (installed-libraries))))))

  (define (load filename)
    (apply load-r6rs-top-level filename 'load (cdr (command-line)))
    (void))
    
  (define (ironscheme-test)
    (load "tests/r6rs/run.sps"))    
      
  (define ironscheme-build
    (case-lambda
      [()      (ironscheme-build #f)]
      [(cps?)  
        ;(call-with-output-file "build-options.ss"
          ;(lambda (p)
            ;(write `(define-option cps-mode ,cps?) p)
            ;(write `(define-option if-wants-letrec* ,(not cps?)) p)
            ;(newline p)))
        (load "ironscheme-buildscript.ss")]))
    
  (define foreground-color
    (case-lambda
      [()           (and (not (emacs-mode?)) (clr-static-prop-get Console ForegroundColor))]
      [(color)      (and (not (emacs-mode?)) (clr-static-prop-set! Console ForegroundColor color))])) 
      
  (define (system-exception? e)
    (clr-is SystemException e))  
    
  (define (eval-top-level x)
    (clr-guard [e [e (parameterize ((foreground-color 'red)
                                    (current-output-port (current-error-port)))
                       (display "Unhandled CLR exception during evaluation:\n")
                       (display e)
                       (newline))]]
      (call/cc
        (lambda (k)
          (with-exception-handler
            (lambda (e)
              (let ((serious? (or (serious-condition? e) (system-exception? e) (not (condition? e)))))
                (parameterize ((foreground-color (if serious? 'red 'yellow))
                               (current-output-port (current-error-port)))
                  (when serious?
                    (display "Unhandled exception during evaluation:\n"))
                  (display e)
                  (newline))
                (k)))
            (lambda ()
              (parameterize ([allow-library-redefinition #t])
                (eval x (interaction-environment)))))))))
            
  (define (eval-embedded x)
    (eval x (interaction-environment)))            
    
  (define compile-system-libraries
    (case-lambda
      [()
        (eval-top-level 
          `(begin
             (include "system-libraries.ss")
             (compile "system-libraries.ss")))]
      [(filename)
        (when (file-exists? filename)
          (delete-file filename))
        (parameterize [(serializer-port (open-file-output-port filename))]
          (eval-top-level 
            `(begin
               (include "system-libraries.ss")
               (compile "system-libraries.ss"))))]))
    
  (define (compile filename)
    (load-r6rs-top-level filename 'compile))
    
  (define (compile->closure filename)
    (load-r6rs-top-level filename 'closure))
    
  (define (load-port port . args)
    (load-port-r6rs-top-level port #f 'load args)
    (void))
    
  (define (->bytes n)
    (let ((bv (make-bytevector 4)))
      (bytevector-u32-native-set! bv 0 4 n)
      bv))
      
  (define (load-port-r6rs-top-level port close-port? how args)
    (let ((x* (let f ()
                (let ((x (read-annotated port)))
                   (if (eof-object? x) 
                       '()
                       (cons x (f)))))))
      (when close-port?
        (close-input-port port))
      (case how
        ((closure)   (pre-compile-r6rs-top-level x*))
        ((load)      
          (parameterize ([command-line (cons (format "~a" port) (map (lambda (x) (format "~a" x)) args))])
            ((compile-r6rs-top-level x*))))
        ((compile)   
            (let ((sp (serializer-port))
                  (extracter #f))
              (when sp
                (let-values (((p e) (open-bytevector-output-port)))
                  (serializer-port p)
                  (set! extracter e))
                (library-map (make-eq-hashtable)))
				      (compile-r6rs-top-level x*) ; i assume this is needed
				      (serialize-all serialize-library compile-core-expr)
			        (when sp
  	            (serialize-port (library-map) sp)
 		            (put-bytevector sp (extracter))
  		          (close-output-port sp))
				      )))))
    
  (define (load-r6rs-top-level filename how . args)
    (parameterize ([library-path (local-library-path filename)])
      (let ((x* 
             (call-with-input-file filename
               (lambda (p)
                 (let f ()
                   (let ((x (read-annotated p)))
                     (if (eof-object? x) 
                         '()
                         (cons x (f)))))))))
        (case how
          ((closure)   (pre-compile-r6rs-top-level x*))
          ((load)      
            (parameterize ([command-line (cons filename (map (lambda (x) (format "~a" x)) args))])
              ((compile-r6rs-top-level x*))))
          ((compile)   
            (let ((sp (serializer-port))
                  (extracter #f))
              (when sp
                (let-values (((p e) (open-bytevector-output-port)))
                  (serializer-port p)
                  (set! extracter e))
                (library-map (make-eq-hashtable)))
				      (compile-r6rs-top-level x*) ; i assume this is needed
				      (serialize-all serialize-library compile-core-expr)
			        (when sp
  	            (serialize-port (library-map) sp)
		            (put-bytevector sp (extracter))
  		          (close-output-port sp))))))))

  (define fo (make-enumeration '(no-fail no-create no-truncate)))
  
  (define (compile-library-content sk 
          id name ver imp* vis* inv* exp-subst exp-env
          visit-proc invoke-proc guard-proc guard-req* visible?)
    (sk id name ver imp* vis* inv* exp-subst exp-env
        (lambda () ((compile-core (expanded->core visit-proc))))
        (compile-core (expanded->core invoke-proc))
        (compile-core (expanded->core guard-proc))
        guard-req* visible?))
        
  (define (load-fasl filename sk)        
    (let ((fasl-filename (change-extension filename)))
      (if (file-exists? fasl-filename)
          (if (or (not (file-exists? filename))
                  (file-newer? fasl-filename filename))
              (let ((port (open-file-input-port fasl-filename)))                  
                (clr-guard [e 
                        (e 
                          (close-input-port port)
                          (display (format "WARNING: precompiled library (~a) could not load.\n" (relative-filename filename)) (current-error-port))
                          #f)]
                  (let ((content (deserialize-port port)))
                    (close-input-port port)
                    (apply compile-library-content sk content))))
              (begin
                (display (format "WARNING: precompiled library (~a) is out of date.\n" (relative-filename filename)) (current-error-port))
                #f))
          #f)))
          
  (define (load-from-package pkg filename sk)
    (let* ((port ((open-package)))
           (os (hashtable-ref pkg filename #f))
           (sos (car os))
           (eos (cdr os)))
      (set-port-position! port sos)
      (let* ((lp (open-bytevector-input-port (get-bytevector-n port ($fx- eos sos))))
             (content (deserialize-port lp)))
        (close-input-port port)
        (close-input-port lp)
        (apply compile-library-content sk content))))          
  
  (define (load-serialized-library filename sk)
    (let ((pkg (package-map)))
      (if pkg
          (or (load-from-package pkg filename sk)
              (load-fasl filename sk))
          (load-fasl filename sk))))
    
  (define (change-extension filename)
    (clr-static-call System.IO.Path ChangeExtension filename ".fasl"))  
    
  (define (serialize-library filename content)
    (display "serializing ")
    (display (relative-filename filename))
    (newline)
    (let ((sp (serializer-port)))
      (if sp
          (let ((lm (library-map))
                (sos (port-position sp)))
            (let-values (((op e) (open-bytevector-output-port)))
              (serialize-port content op)
              (put-bytevector sp (e))
              (hashtable-set! lm (format "~a" (cadr content)) (cons sos (port-position sp)))))
          (begin    
            (let ((fasl-filename (change-extension filename)))
              (when (file-exists? fasl-filename)
                (delete-file fasl-filename))
            (let ((port (open-file-output-port fasl-filename)))
              (serialize-port content port)
              (close-output-port port)))))))

  (current-precompiled-library-loader load-serialized-library)
  
  (initialize-default-printers)
  
  (set-symbol-value! 'default-exception-handler 
    (lambda (ex)
      (cond
        [(serious-condition? ex) (raise ex)]
        [else 
          (display ex)
          (newline)])))
      
  (set-symbol-value! 'load load)
  (set-symbol-value! 'compile compile)
  (set-symbol-value! 'compile->closure compile->closure)
  (set-symbol-value! 'eval-r6rs eval-top-level)
  (set-symbol-value! 'eval-embedded eval-embedded)
  (set-symbol-value! 'int-env-syms interaction-environment-symbols)
  (set-symbol-value! 'expanded2core expanded->core)
  
  (set-symbol-value! 'trace-printer trace-printer)
  (set-symbol-value! 'pretty-print pretty-print)
  (cps-mode
    (set-symbol-value! 'convert->cps convert->cps)
    (void))
  (set-symbol-value! 'assertion-violation assertion-violation)
  (set-symbol-value! 'raise raise)
  (set-symbol-value! 'raise-continuable raise-continuable)
  (set-symbol-value! 'with-exception-handler with-exception-handler)
  (set-symbol-value! 'emacs-mode? emacs-mode?)
  (set-symbol-value! 'stacktrace-enable? display-stacktrace)
  
  (file-options-constructor (enum-set-constructor fo))
  
  (library-path (get-library-paths))
  
  (library-extensions (cons ".ironscheme.sls" (library-extensions)))
  
  (unless (debug-mode?)
    (enable-constant-fold/env 
      '(only (rnrs) + - * / = < > <= >= negative? positive? zero? exp expt div mod div0 mod0 even? odd?)
      '(except (rnrs arithmetic fixnums) fx*/carry fx-/carry fx+/carry fxdiv0-and-mod0 fxdiv-and-mod)
      '(except (rnrs arithmetic flonums) fldiv0-and-mod0 fldiv-and-mod 
                make-no-infinities-violation make-no-nans-violation 
                no-infinities-violation? no-nans-violation?)
      '(rnrs arithmetic bitwise)))
  
  (interaction-environment (new-interaction-environment))
  )

