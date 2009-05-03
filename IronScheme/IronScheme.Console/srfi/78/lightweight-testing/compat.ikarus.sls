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

(library (srfi :78 lightweight-testing compat)
  (export
    pretty-print/no-trailing-newline)
  (import
    (rnrs)
    (only (ikarus) pretty-print))
  
  ;; check.scm says a pretty-print with a trailing newline 
  ;; will make its print-outs look bad, so:
  (define pretty-print/no-trailing-newline
    (case-lambda
      [(datum output-port)
       (let* ([os (call-with-string-output-port (lambda (sop) (pretty-print datum sop)))]
              [os (if (and (positive? (string-length os))
                           (char=? #\newline (string-ref os (- (string-length os) 1))))
                    (substring os 0 (- (string-length os) 1))
                    os)])
         (display os output-port))]
      [(datum) 
       (pretty-print/no-trailing-newline datum (current-output-port))]))  
)
