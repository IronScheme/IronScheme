#!r6rs
;;; condition.sls --- Deque Conditions

;; Copyright (C) 2013 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.
(library (pfds deques private condition)
(export &deque-empty
        make-deque-empty-condition
        deque-empty-condition?)
(import (rnrs conditions))

(define-condition-type &deque-empty
  &assertion
  make-deque-empty-condition
  deque-empty-condition?)

)
