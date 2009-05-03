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

#!r6rs
(library (srfi :37 args-fold)
  (export
    args-fold
    (rename (make-option option))
    option?
    option-names
    option-required-arg?
    option-optional-arg?
    option-processor)
  (import 
    (rnrs)
    (srfi private include))
  
  
  (define-record-type option
    (fields 
      names required-arg? optional-arg? processor)
    (protocol 
      (lambda (c) 
        (lambda (n ra oa p)
          (if (and 
                (and (list? n)
                     (positive? (length n))
                     (for-all (lambda (x) 
                                (or (and (string? x) (positive? (string-length x))) 
                                    (char? x))) 
                              n))
                (boolean? ra)
                (boolean? oa)
                (not (and ra oa))
                (procedure? p))
            (c n ra oa p)
            (assertion-violation 'option "invalid arguments" n ra oa p))))))

  (define args-fold
    (let ([option make-option])
      (include/resolve ("srfi" "37") "srfi-37-reference.scm")
      args-fold))
)
