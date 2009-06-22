#!r6rs
#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

(library (ironscheme record-case)
  (export
    else
    record-case)
  (import 
    (rnrs))

  (define (get-index vec sym)
    (let ((len (vector-length vec)))
      (let f ((i 0))
        (cond 
          [(= i len) #f]
          [(eq? sym (vector-ref vec i)) i]
          [else (f (+ i 1))]))))

  (define (get-fields r rtd . x)  
    (reverse
      (let ((names (record-type-field-names rtd)))
        (let f ((x x)(o '()))
          (if (null? x)
            o
            (let g ((i (get-index names (car x)))(rtd rtd))
              (if i
                (f (cdr x) (cons ((record-accessor rtd i) r) o))
                (let ((prtd (record-type-parent rtd)))
                  (if prtd
                    (g (get-index (record-type-field-names prtd) (car x)) prtd)
                    (assertion-violation 'get-fields "field not found" (car x) r))))))))))

  (define-syntax record-case
    (lambda (x)
      (syntax-case x (else)
        [(_ r [(r? f ...) e e* ...] ... [else ee])
          (for-all identifier? #'(r? ...))
          #'(let ((r* r))
              (cond
                [((record-predicate (record-type-descriptor r?)) r*)
                 (apply 
                  (lambda (f ...) e e* ...) 
                  (get-fields r* (record-type-descriptor r?) 'f ...) )] ...
                [else ee]))]
        [(_ r [(r? f ...) e  e* ...] ... )
          #'(record-case r [(r? f ...) e e* ...] ... [else #f])]
          )))
)
    