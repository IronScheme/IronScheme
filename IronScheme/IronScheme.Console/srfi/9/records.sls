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
(library (srfi :9 records)
  (export 
    (rename (srfi:define-record-type define-record-type)))
  (import 
    (rnrs))
  
  (define-syntax srfi:define-record-type
    (lambda (stx)
      (syntax-case stx ()
        [(_ type (constructor constructor-tag ...)
                 predicate
                 (field-tag accessor setter ...) ...)         
         (and (for-all identifier? 
                       #'(type constructor predicate constructor-tag ... 
                               field-tag ... accessor ...))
              (for-all (lambda (s) 
                         (or (and (= 1 (length s)) (identifier? (car s)))
                             (= 0 (length s))))
                       #'((setter ...) ...))
              (for-all (lambda (ct) 
                         (memp (lambda (ft) (bound-identifier=? ct ft))
                               #'(field-tag ...)))
                       #'(constructor-tag ...)))         
         (with-syntax ([(field-clause ...)
                        (map (lambda (clause)
                               (if (= 2 (length clause)) 
                                 #`(immutable . #,clause) 
                                 #`(mutable . #,clause)))
                             #'((field-tag accessor setter ...) ...))]
                       [(unspec-tag ...)
                        (remp (lambda (ft) 
                                (memp (lambda (ct) (bound-identifier=? ft ct))
                                      #'(constructor-tag ...)))
                              #'(field-tag ...))])
           #'(define-record-type (type constructor predicate)
               (sealed #t)
               (protocol (lambda (ctor)
                           (lambda (constructor-tag ...)
                             (define unspec-tag)
                             ...
                             (ctor field-tag ...))))
               (fields field-clause ...)))])))
  
)
