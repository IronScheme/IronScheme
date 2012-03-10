#!r6rs
;;; fmt-js.sls -- fmt-js for R6RS
;;
;; Copyright (c) 2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt
;;
;; R6RS adaption Copyright (c) 2011 Andreas Rottmann, same license.

(library (wak fmt js)
 (export js-expr js-function js-var js-comment js-array js-object js=== js>>>)
 (import (rnrs base)
         (wak private include)
         (wak fmt)
         (wak fmt c))
 
 (include-file ((wak fmt private) fmt-js)))
