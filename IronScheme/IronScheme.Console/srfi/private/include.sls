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
(library (srfi private include)
  (export 
    include/resolve)
  (import 
    (rnrs) 
    (for (srfi private include compat) expand))
  
  (define-syntax include/resolve
    (lambda (stx)
      (define (include/lexical-context ctxt filename)
        (with-exception-handler
          (lambda (ex)
            (raise
             (condition
              (make-error)
              (make-who-condition 'include/resolve)
              (make-message-condition "error while trying to include")
              (make-irritants-condition (list filename))
              (if (condition? ex) ex (make-irritants-condition (list ex))))))
          (lambda ()
            (call-with-input-file filename
              (lambda (fip)
                (let loop ([a '()])
                  (let ([x (read fip)])
                    (if (eof-object? x)
                      (datum->syntax ctxt `(begin . ,(reverse a)))
                      (loop (cons x a))))))))))
      (syntax-case stx ()
        [(ctxt (lib-path* ...) file-path)
         (for-all (lambda (s) (and (string? s) (positive? (string-length s)))) 
                  (syntax->datum #'(lib-path* ... file-path)))
         (let ([p (apply string-append 
                         (map (lambda (ps) (string-append "/" ps)) 
                              (syntax->datum #'(lib-path* ... file-path))))]
               [sp (search-paths)])
           (let loop ([search sp])
             (if (null? search)
               (error 'include/resolve "cannot find file in search paths"
                      (substring p 1 (string-length p)) sp)
               (let ([full (string-append (car search) p)])
                 (if (file-exists? full)
                   (include/lexical-context #'ctxt full)
                   (loop (cdr search)))))))])))
)
