#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(import
  (rnrs)
  (srfi :25 multi-dimensional-arrays)
  (srfi :78 lightweight-testing)
  (srfi private include))

(let-syntax ((or
              (syntax-rules (error)
                ((_ expr (error msg))
                 (check (and expr #T) => #T))
                ((_ . r) (or . r))))
             (past
              (syntax-rules ()
                ((_ . r) (begin)))))
  (include/resolve ("srfi" "25") "test.scm"))

(check-report)
