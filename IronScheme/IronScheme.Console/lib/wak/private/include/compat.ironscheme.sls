;;; compat.ironscheme.sls --- include compatibility for ironscheme

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

(library (wak private include compat)
  (export stale-when
          annotation?
          annotation-expression
          read-annotated
          file-mtime
          merge-path
          (rename (library-path library-search-paths)))
  (import (rnrs)
          (wak private include utils)
          (ironscheme clr)
          (only (ironscheme strings) string-replace)
          (only (ironscheme reader) 
                annotation?
                read-annotated
                annotation-expression)
          (only (ironscheme)
                library-path
                stale-when
                file-mtime))
                
(define (merge-path path origin)
  (string-replace
    (string-replace 
      (string-append origin "/" (string-join path "/"))
      (clr-static-prop-get IronScheme.Runtime.Builtins ApplicationDirectory)
      ".")
     "\\"
     "/"))) ; make precompiled assemblies work on Linux
    
