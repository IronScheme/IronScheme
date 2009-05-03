;; Copyright (c) 2009 Derick Eddington
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; Except as contained in this notice, the name(s) of the above copyright
;; holders shall not be used in advertising or otherwise to promote the sale,
;; use or other dealings in this Software without prior written authorization.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;; Inspired by Danny Yoo's get-environment PLaneT package.

#!r6rs
(library (srfi :98 os-environment-variables)
  (export
    (rename (getenv get-environment-variable))
    get-environment-variables)
  (import
    (rnrs base)
    (only (scheme base) getenv)
    (scheme foreign))

  (unsafe!)

  (define environ (get-ffi-obj "environ" (ffi-lib #F) _pointer))

  (define (get-environment-variables)
    (let loop ((i 0) (accum '()))
      (let ((next (ptr-ref environ _string/locale i)))
        (if next
          (loop (+ 1 i)
                (cons (let loop ((i 0) (len (string-length next)))
                        (if (< i len)
                          (if (char=? #\= (string-ref next i))
                            (cons (substring next 0 i)
                                  (substring next (+ 1 i) len))
                            (loop (+ 1 i) len))
                          (cons next #F)))
                      accum))
          accum))))
)
