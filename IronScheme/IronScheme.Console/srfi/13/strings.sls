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
(library (srfi :13 strings)
  (export
    string-map string-map!
    string-fold       string-unfold
    string-fold-right string-unfold-right 
    string-tabulate string-for-each string-for-each-index
    string-every string-any
    string-hash string-hash-ci
    string-compare string-compare-ci
    string=    string<    string>    string<=    string>=    string<>
    string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<> 
    string-downcase  string-upcase  string-titlecase  
    string-downcase! string-upcase! string-titlecase! 
    string-take string-take-right
    string-drop string-drop-right
    string-pad string-pad-right
    string-trim string-trim-right string-trim-both
    string-filter string-delete
    string-index string-index-right 
    string-skip  string-skip-right
    string-count
    string-prefix-length string-prefix-length-ci
    string-suffix-length string-suffix-length-ci
    string-prefix? string-prefix-ci?
    string-suffix? string-suffix-ci?
    string-contains string-contains-ci
    string-copy! substring/shared
    string-reverse string-reverse! reverse-list->string
    string-concatenate string-concatenate/shared string-concatenate-reverse
    string-concatenate-reverse/shared
    string-append/shared
    xsubstring string-xcopy!
    string-null?
    string-join
    string-tokenize
    string-replace
    ; R5RS extended:
    string->list string-copy string-fill! 
    ; R5RS re-exports:
    string? make-string string-length string-ref string-set! 
    string string-append list->string
    ; Low-level routines:
    #;(make-kmp-restart-vector string-kmp-partial-search kmp-step
    string-parse-start+end
    string-parse-final-start+end
    let-string-start+end
    check-substring-spec
    substring-spec-ok?)
    )
  (import
    (except (rnrs) error string-copy string-for-each string->list
                   string-upcase string-downcase string-titlecase string-hash)
    (except (rnrs mutable-strings) string-fill!)
    (rnrs r5rs)
    (prefix (srfi :23 error) ER:)
    (srfi :8 receive)
    (srfi :14 char-sets)
    (srfi :39 parameters)
    (srfi private let-opt)
    (srfi private include))
  
  (define (error . args)
    (parameterize ([ER:error-who 
                    "(library (srfi :13 strings))"])
      (apply ER:error args)))
  
  (define-syntax check-arg
    (lambda (stx)
      (syntax-case stx ()
        [(_ pred val caller)
         (and (identifier? #'val) (identifier? #'caller))
         #'(unless (pred val)
             (parameterize ([ER:error-who 'caller])
               (ER:error "check-arg failed" val)))])))
  
  (define (char-cased? c)
    (char-upper-case? (char-upcase c)))
  
  (include/resolve ("srfi" "13") "srfi-13.scm")
)
