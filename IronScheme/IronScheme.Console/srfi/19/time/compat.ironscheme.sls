#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(library (srfi :19 time compat)
  (export
    format
    host:time-resolution
    host:current-time 
    host:time-nanosecond 
    host:time-second 
    host:time-gmt-offset)
  (import
    (rnrs base)
    (only (ironscheme) format)
    (ironscheme datetime))
  
  (define host:time-resolution 1000)
  
  (define base (datetime->local (make-utc-datetime 1970 1 1)))
  
  (define (host:current-time) (now))
  ; since 1970
  (define (host:time-nanosecond t) (exact (round (* 1000 (total-milliseconds (difference t base))))))
  (define (host:time-second t) (exact (round (total-seconds (difference t base)))))
  (define (host:time-gmt-offset t) (exact (round (total-seconds (difference t (datetime->utc t))))))
)
