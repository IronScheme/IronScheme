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
(library (srfi :2 and-let*)  
  (export 
    and-let*)
  (import 
    (rnrs))
  
  (define-syntax and-let*
    (lambda (stx)
      (define (get-id c)
        (syntax-case c () [(var expr) #'var] [_ #f]))
      (syntax-case stx ()
        [(_ (clause* ...) body* ...)
         (for-all identifier? (filter values (map get-id #'(clause* ...))))
         #'(and-let*-core #t (clause* ...) body* ...)])))
  
  (define-syntax and-let*-core
    (lambda (stx)
      (syntax-case stx ()
        [(kw _ ([var expr] clause* ...) body* ...)
         #'(let ([var expr])
             (if var
               (kw var (clause* ...) body* ...)
               #f))]
        [(kw _ ([expr] clause* ...) body* ...)
         #'(let ([t expr])
             (if t
               (kw t (clause* ...) body* ...)
               #f))]
        [(kw _ (id clause* ...) body* ...)
         (or (identifier? #'id)
             (syntax-violation #f "invalid clause" stx #'id))
         #'(if id
             (kw id (clause* ...) body* ...)
             #f)]
        [(kw last () body* ...)
         (if (positive? (length #'(body* ...)))
           #'(begin body* ...)
           #'last)])))
)
