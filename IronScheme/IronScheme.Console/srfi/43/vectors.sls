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
(library (srfi :43 vectors)
  (export
    ;;; * Constructors
    make-vector vector
    vector-unfold         vector-unfold-right
    vector-copy           vector-reverse-copy
    vector-append         vector-concatenate
    ;;; * Predicates
    vector?
    vector-empty?
    vector=
    ;;; * Selectors
    vector-ref
    vector-length
    ;;; * Iteration
    vector-fold           vector-fold-right
    vector-map            vector-map!
    vector-for-each
    vector-count
    ;;; * Searching
    vector-index          vector-skip
    vector-index-right    vector-skip-right
    vector-binary-search  vector-any    vector-every
    ;;; * Mutators
    vector-set!
    vector-swap!
    vector-fill!
    vector-reverse!
    vector-copy!          vector-reverse-copy!
    ;;; * Conversion
    vector->list          reverse-vector->list
    list->vector          reverse-list->vector )
  (import
    (except (rnrs) error vector-map vector-for-each vector-fill! vector->list
                   list->vector)
    (prefix (only (rnrs) vector-fill! vector->list list->vector) rnrs:)
    (rnrs r5rs)
    (prefix (srfi :23 error) ER:)
    (srfi :39 parameters)
    (srfi :8 receive)
    (srfi private include))
  
  (define (error . args)
    (parameterize ([ER:error-who 
                    "(library (srfi :43 vectors))"])
      (apply ER:error args)))
  
  (define-syntax check-type
    (lambda (stx)
      (syntax-case stx ()
        [(_ pred? value callee)
         (if (identifier? #'value)
           #'(if (pred? value)
               value
               (parameterize ([ER:error-who callee])
                 (ER:error "erroneous value" value)))
           #'(let ([v value])
               (if (pred? v)
                 v
                 (parameterize ([ER:error-who callee])
                   (ER:error "erroneous value" v)))))])))
    
  (include/resolve ("srfi" "43") "vector-lib.scm")
)
