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
(import 
  (rnrs)
  (rnrs eval)
  (srfi :2 and-let*)
  (srfi :78 lightweight-testing))

(define-syntax expect
  (syntax-rules ()
    [(_ expr result)
     (check expr => result)]))

(define-syntax must-be-a-syntax-error
  (syntax-rules ()
    [(_ expr)
     (check 
       (guard (ex [#t (syntax-violation? ex)])  
         (eval 'expr (environment '(rnrs) '(srfi :2 and-let*))))
       => #t)]))

;; Taken straight from the reference implementation tests

(expect  (and-let* () 1) 1)
(expect  (and-let* () 1 2) 2)
(expect  (and-let* () ) #t)

(expect (let ((x #f)) (and-let* (x))) #f)
(expect (let ((x 1)) (and-let* (x))) 1)
(expect (and-let* ((x #f)) ) #f)
(expect (and-let* ((x 1)) ) 1)
(must-be-a-syntax-error (and-let* ( #f (x 1))) )
(expect (and-let* ( (#f) (x 1)) ) #f)
(must-be-a-syntax-error (and-let* (2 (x 1))) )
(expect (and-let* ( (2) (x 1)) ) 1)
(expect (and-let* ( (x 1) (2)) ) 2)
(expect (let ((x #f)) (and-let* (x) x)) #f)
(expect (let ((x "")) (and-let* (x) x)) "")
(expect (let ((x "")) (and-let* (x)  )) "")
(expect (let ((x 1)) (and-let* (x) (+ x 1))) 2)
(expect (let ((x #f)) (and-let* (x) (+ x 1))) #f)
(expect (let ((x 1)) (and-let* (((positive? x))) (+ x 1))) 2)
(expect (let ((x 1)) (and-let* (((positive? x))) )) #t)
(expect (let ((x 0)) (and-let* (((positive? x))) (+ x 1))) #f)
(expect (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1))) (+ x 1)))  3)
;; This next one is from the reference implementation tests
;; but I can't see how it "must be a syntax-error".
#;(must-be-a-syntax-error
  (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1))))

(expect (let ((x 1)) (and-let* (x ((positive? x))) (+ x 1))) 2)
(expect (let ((x 1)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))) 2)
(expect (let ((x 0)) (and-let* (x ((positive? x))) (+ x 1))) #f)
(expect (let ((x #f)) (and-let* (x ((positive? x))) (+ x 1))) #f)
(expect (let ((x #f)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))) #f)

(expect  (let ((x 1)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  (let ((x 0)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  (let ((x #f)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  (let ((x 3)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) 3/2)

(check-report)
