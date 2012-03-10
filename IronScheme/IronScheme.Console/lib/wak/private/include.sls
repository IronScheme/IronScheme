;;; include.sls --- `include' macros

;; Copyright (C) 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the MIT/X11 license.

;; You should have received a copy of the MIT/X11 license along with
;; this program. If not, see
;; <http://www.opensource.org/licenses/mit-license.php>.

;;; Commentary:

;;; Code:
#!r6rs

(library (wak private include)
  (export include-file
          include-file/downcase)
  (import (rnrs)
          (for (wak private include helper) expand)
          (for (wak private include compat) expand))

(define-syntax include-file
  (lambda (stx)
    (syntax-case stx ()
      ((k <path>)
       (include-file/aux 'include-file #'k (syntax->datum #'<path>) values))
      ((k <directory> <file>)
       (include-file/aux 'include-file
                         #'k
                         (syntax->datum #'(<directory> <file>))
                         values)))))

(define-syntax include-file/downcase
  (lambda (stx)
    ;; This loses all the annotations, but Ikarus provides no way to
    ;; (re-)construct annotation objects ATM.
    (define (downcase thing)
      (let ((form (if (annotation? thing)
                      (annotation-expression thing)
                      thing)))
        (cond ((symbol? form)
               (string->symbol (string-downcase (symbol->string form))))
              ((pair? form)
               (cons (downcase (car form))
                     (downcase (cdr form))))
              (else
               thing))))
    (syntax-case stx ()
      ((k <path>)
       (include-file/aux 'include-file #'k (syntax->datum #'<path>) downcase))
      ((k <directory> <file>)
       (include-file/aux 'include-file
                         #'k
                         (syntax->datum #'(<directory> <file>))
                         downcase)))))

)
