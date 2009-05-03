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
(library (srfi :0 cond-expand)
  (export 
    cond-expand)
  (import
    (rnrs)
    (for (srfi private registry) expand))
  
  (define-syntax cond-expand
    (lambda (stx)
      (syntax-case stx (and or not else)
        [(_) 
         (syntax-violation #f "Unfulfilled cond-expand" stx)]
        [(_ (else body ...))
         #'(begin body ...)]
        [(_ ((and) body ...) more-clauses ...)
         #'(begin body ...)]
        [(_ ((and req1 req2 ...) body ...) more-clauses ...)
         #'(cond-expand
            (req1
             (cond-expand
              ((and req2 ...) body ...)
              more-clauses ...))
            more-clauses ...)]
        [(_ ((or) body ...) more-clauses ...)
         #'(cond-expand more-clauses ...)]
        [(_ ((or req1 req2 ...) body ...) more-clauses ...)
         #'(cond-expand
            (req1
             (begin body ...))
            (else
             (cond-expand
              ((or req2 ...) body ...)
              more-clauses ...)))]
        [(_ ((not req) body ...) more-clauses ...)
         #'(cond-expand
            (req
             (cond-expand more-clauses ...))
            (else body ...))]
        [(_ (feature-id body ...) more-clauses ...)
         (if (member (syntax->datum #'feature-id) available-features)
           #'(begin body ...)
           #'(cond-expand more-clauses ...))])))
  
)
