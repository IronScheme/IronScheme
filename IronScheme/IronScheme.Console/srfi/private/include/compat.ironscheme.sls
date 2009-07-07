#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (srfi private include compat)
  (export
    search-paths)
  (import
    (rnrs base)
    (only (ironscheme) library-path))

  (define (search-paths)
    (library-path))
)
