;;; filesys.sls --- Filsystem utilities for `include'

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

(library (wak private include filesys)
  (export find-file)
  (import (rnrs base)
          (rnrs files)
          (wak private include compat))

(define (find-file path origins)
  (let loop ((origins origins))
    (if (null? origins)
        #f
        (let ((filename (merge-path path (car origins))))
          (if (file-exists? filename)
              filename
              (loop (cdr origins)))))))

)
