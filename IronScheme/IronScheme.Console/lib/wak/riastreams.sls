;; Copyright (C) 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the MIT/X11 license.

;; You should have received a copy of the MIT/X11 license along with
;; this program. If not, see
;; <http://www.opensource.org/licenses/mit-license.php>.


#!r6rs

(library (wak riastreams)
  (export stream-cons
          stream-nil
          stream-null?
          stream-pair?
          stream-car
          stream-cdr
          stream->list
          list->stream
          string->stream
          vector->stream
          stream-difference
          stream-append
          
          in-stream)
  (import (rnrs)
          (srfi :45 lazy)
          (wak private include))

  (include-file/downcase ((wak riastreams private) stream)))
