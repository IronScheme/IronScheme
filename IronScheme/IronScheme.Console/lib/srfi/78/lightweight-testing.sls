;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs
(library (srfi :78 lightweight-testing)
  (export
    check
    check-ec
    check-report
    check-set-mode!
    check-reset!
    check-passed?)
  (import 
    (rnrs)
    (srfi :78 lightweight-testing compat)
    (srfi :39 parameters)
    (srfi private include)
    (srfi :23 error tricks)
    (srfi :42 eager-comprehensions))
  
  (SRFI-23-error->R6RS "(library (srfi :78 lightweight-testing))"
   (include/resolve ("srfi" "78") "check.scm"))
)
