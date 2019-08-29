#!r6rs
;;; bitwise.sls --- Bitwise Arithmetic Utilities

;; Copyright (C) 2014 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.
(library (pfds private bitwise)
(export bitwise-bit-set
        bitwise-bit-unset
        )
(import (rnrs base)
        (rnrs arithmetic bitwise))

(define (bitwise-bit-set bits i)
  (bitwise-ior bits (bitwise-arithmetic-shift-left 1 i)))

(define (bitwise-bit-unset bits i)
  (bitwise-and bits (bitwise-not (bitwise-arithmetic-shift-left 1 i))))

)
