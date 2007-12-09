;;; Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

(library (psyntax config)
  (export if-wants-define-struct if-wants-case-lambda
          if-wants-letrec* if-wants-global-defines)
  (import (rnrs))
  (define-syntax define-option
    (syntax-rules ()
      ((_ name #t) 
       (define-syntax name 
         (syntax-rules ()
           ((_ sk fk) sk))))
      ((_ name #f) 
       (define-syntax name 
         (syntax-rules ()
           ((_ sk fk) fk))))))
  
  (define-option if-wants-define-struct  #f)
  ;;; define-record is an ikarus-specific extension.
  ;;; should be disabled for all other implementations
  ;;; the source is included to illustrate how
  ;;; implementation-specific extensions can be added 
  ;;; to the expander
  
  (define-option if-wants-global-defines #f)
  ;;; If the implementation requires that all global 
  ;;; variables be defined before they're set!ed, 
  ;;; then enabling this option causes the expander
  ;;; to produce (define <global> '#f) for every 
  ;;; exported identifiers.  If the option is disabled, 
  ;;; then the global definitions are suppressed.
  
  (define-option if-wants-case-lambda    #t)
  ;;; Implementations that support case-lambda natively
  ;;; should have the next option enabled.  Disabling
  ;;; wants-case-lambda causes the expander to produce 
  ;;; ugly, inefficient, but correct code by expanding
  ;;; case-lambda into explicit dispatch code.

  (define-option if-wants-letrec*        #t)
  ;;; If the implementation has built-in support for
  ;;; efficient letrec* (ikarus, chez), then this option
  ;;; should be enabled.  Disabling the option expands
  ;;; (letrec* ([lhs* rhs*] ...) body) into
  ;;; (let ([lhs* #f] ...) (set! lhs* rhs*) ... body)
)
