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

;; force library loading
queues
deques
bbtrees
sets
psqs
heaps
fingertrees
sequences
hamts

(run-test pfds)
