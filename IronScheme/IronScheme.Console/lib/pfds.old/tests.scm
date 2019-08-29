#!r6rs
;; Copyright (C) 2011-2014 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Code:
(import (rnrs)
        (pfds tests queues)
        (pfds tests deques)
        (pfds tests bbtrees)
        (pfds tests sets)
        (pfds tests psqs)
        (pfds tests heaps)
        (pfds tests fingertrees)
        (pfds tests sequences)
        (pfds tests hamts)
        (pfds tests utils)
        (wak trc-testing))

;; Some schemes use lazy loading of modules, and so I can't just use
;; (run-test pfds) and rely on the side effects in the other modules
;; to add them to the pfds parent suite.
(define-syntax add-tests!
  (syntax-rules ()
    ((add-tests! suite ...)
     (begin (add-test! pfds 'suite suite) ...))))

(add-tests! queues deques bbtrees sets psqs
            heaps fingertrees sequences hamts)

(run-test pfds)
