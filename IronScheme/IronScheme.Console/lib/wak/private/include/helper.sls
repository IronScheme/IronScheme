;;; helper.sls --- Helper procedures for (wak private include)

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the MIT/X11 license.

;; You should have received a copy of the MIT/X11 license along with
;; this program. If not, see
;; <http://www.opensource.org/licenses/mit-license.php>.

;;; Commentary:

;;; Code:
#!r6rs

(library (wak private include helper)
  (export include-file/aux)
  (import (rnrs)
          (wak private include utils)
          (wak private include filesys)
          (for (wak private include compat) run (meta -1)))

(define (error/conditions who msg irrts . cndts)
  (raise
    (apply condition
           (make-error)
           (make-who-condition who)
           (make-message-condition msg)
           (make-irritants-condition irrts)
           cndts)))

(define (maybe-symbol->string thing)
  (cond ((symbol? thing) (symbol->string thing))
        (else            thing)))

(define (filespec->path name)
  (cond ((string? name) (list name))
        ((symbol? name) (list (string-append (symbol->string name) ".scm")))
        ((pair? name)
         (append
          (if (pair? (car name))
              (map maybe-symbol->string (car name))
              (list (maybe-symbol->string (car name))))
          (list (cond ((symbol? (cadr name))
                       (string-append (symbol->string (cadr name)) ".scm"))
                      (else
                       (cadr name))))))
        (else (list name))))

(define (include-file/aux who ctxt path transformer)
  (let* ((relpath (filespec->path path))
         (filename (find-file relpath (library-search-paths))))
    (unless filename
      (error who
             "cannot find library file in search paths"
             relpath
             (library-search-paths)))
    (with-exception-handler
        (lambda (ex)
          (error/conditions who
                            "error while trying to include"
                            (list filename)
                            (if (condition? ex)
                                ex
                                (make-irritants-condition (list ex)))))
      (lambda ()
        (call-with-input-file filename
          (lambda (port)
            (let loop ((x (read-annotated port)) (forms '()))
              (if (eof-object? x)
                  #`(stale-when (and (file-exists? #,filename)
                                     (> (file-mtime #,filename)
                                        #,(file-mtime filename)))
                                #,@(datum->syntax ctxt (reverse forms)))
                  (loop (read-annotated port)
                        (cons (transformer x) forms))))))))))

)

;; Local Variables:
;; scheme-indent-styles: ((stale-when 1))
;; End:
