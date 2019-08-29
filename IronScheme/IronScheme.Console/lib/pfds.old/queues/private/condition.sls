#!r6rs
;;; condition.sls --- Queue Conditions

;; Copyright (C) 2013 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.
(library (pfds queues private condition)
(export &queue-empty
        make-queue-empty-condition
        queue-empty-condition?)
(import (rnrs conditions))

(define-condition-type &queue-empty
  &assertion
  make-queue-empty-condition
  queue-empty-condition?)

)
