;;; color.sls --- R6RS library for fmt-color.scm
;;
;; Based on fmt-scheme48.scm, which is
;; Copyright (c) 2007 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt
;;
;; R6RS adaption Copyright (c) 2009-2011 Andreas Rottmann, same
;; license.
#!r6rs

(library (wak fmt color)
  (export fmt-in-html fmt-color fmt-in-html? fmt-use-html-font?
          fmt-colored fmt-red fmt-blue fmt-green fmt-cyan fmt-yellow
          fmt-magenta fmt-black fmt-white fmt-bold fmt-underline)
  (import (rnrs)
          (wak fmt)
          (wak private include))

  (define arithmetic-shift bitwise-arithmetic-shift)
  
  (include-file ((wak fmt private) fmt-color)))
