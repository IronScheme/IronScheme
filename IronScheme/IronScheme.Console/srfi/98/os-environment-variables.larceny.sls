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

;; NOTE: I believe this currently works only on Linux.
;; NOTE: If Larceny's FFI changes, this may no longer work.

(library (srfi :98 os-environment-variables)
  (export
    get-environment-variable get-environment-variables)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs bytevectors)
    (rnrs io ports)
    (primitives
     foreign-procedure #;foreign-variable foreign-null-pointer? sizeof:pointer
     %peek-pointer %peek8u void*->address ffi/dlopen ffi/dlsym)
    (srfi :0 cond-expand))

  ;; TODO: Will the convenient string converters use the native transcoder in
  ;;       the future?  So that scheme-str->c-str-bv and c-str-ptr->scheme-str
  ;;       won't be needed.

  (define (scheme-str->c-str-bv x)
    (let* ((bv (string->bytevector x (native-transcoder)))
           (len (bytevector-length bv))
           (bv/z (make-bytevector (+ 1 len))))
      (bytevector-copy! bv 0 bv/z 0 len)
      (bytevector-u8-set! bv/z len 0)
      bv/z))

  (define (c-str-ptr->scheme-str x)
    (let loop ((x x) (a '()))
      (let ((b (%peek8u x)))
        (if (zero? b)
          (bytevector->string (u8-list->bytevector (reverse a))
                              (native-transcoder))
          (loop (+ 1 x) (cons b a))))))
  
  (define getenv
    (foreign-procedure "getenv" '(boxed) 'void*))
  
  (define (get-environment-variable name) 
    (unless (string? name)
      (assertion-violation 'get-environment-variable "not a string" name))
    (let ((p (getenv (scheme-str->c-str-bv name))))
      (and p
           (c-str-ptr->scheme-str (void*->address p)))))

  ;; TODO: Will foreign-variable support a pointer type in the future?
  ;;       Would this be the correct way to use it?
  #;(define environ
      (foreign-variable "environ" 'void*))

  ;; TODO: Is (ffi/dlopen "") okay?  It works for me on Ubuntu Linux 8.10.
  (define environ
    (cond-expand
     (linux
      (%peek-pointer (ffi/dlsym (ffi/dlopen "") "environ")))))

  (define (get-environment-variables)
    (define (entry->pair x) 
      (let* ((s (c-str-ptr->scheme-str x))
             (len (string-length s)))
        (let loop ((i 0))
          (if (< i len)
            (if (char=? #\= (string-ref s i))
              (cons (substring s 0 i)
                    (substring s (+ 1 i) len))
              (loop (+ 1 i)))
            (cons s #F)))))
    (let loop ((e environ) (a '()))
      (let ((entry (%peek-pointer e)))
        (if (foreign-null-pointer? entry)
          a
          (loop (+ sizeof:pointer e)
                (cons (entry->pair entry) a))))))
)
