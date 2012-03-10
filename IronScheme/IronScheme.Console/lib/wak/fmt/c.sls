;;; c.sls -- fmt-c for R6RS
;;
;; Based on fmt-c-mzscheme.scm, which is
;; Copyright (c) 2007 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt
;;
;; R6RS adaption Copyright (c) 2009 Andreas Rottmann, same license.

#!r6rs

(library (wak fmt c)
 (export
  fmt-in-macro? fmt-expression? fmt-return? fmt-default-type
  fmt-newline-before-brace? fmt-braceless-bodies?
  fmt-indent-space fmt-switch-indent-space fmt-op fmt-gen
  c-in-expr c-in-stmt c-in-test
  c-paren c-maybe-paren c-type c-literal? c-literal char->c-char
  c-struct c-union c-class c-enum c-attribute c-typedef
  c-expr c-expr/sexp c-apply c-op c-indent c-current-indent-string
  c-wrap-stmt c-open-brace c-close-brace
  c-block c-braced-block c-begin
  c-fun c-var c-prototype c-param c-param-list
  c-while c-for c-if c-switch
  c-case c-case/fallthrough c-default
  c-break c-continue c-return c-goto c-label
  c-static c-const c-extern c-volatile c-auto c-restrict c-inline
  c++ c-- c+ c- c* c/ c% c& c^ c~ c! c&& c<< c>> c== c!=
  c< c> c<= c>= c= c+= c-= c*= c/= c%= c&= c^= c<<= c>>=
  c++/post c--/post c. c->
  c-bit-or c-or c-bit-or=
  cpp-if cpp-ifdef cpp-ifndef cpp-elif cpp-endif cpp-undef
  cpp-include cpp-define cpp-wrap-header cpp-pragma cpp-line
  cpp-error cpp-warning cpp-stringify cpp-sym-cat
  c-comment c-block-comment)

 (import (rnrs base)
         (rnrs unicode)
         (rnrs io simple)
         (rename (only (rnrs lists) remove memq member)
                 (remove remp))
         (only (srfi :1 lists)
               fold
               length+
               filter
               every)
         (srfi :6)
         (only (srfi :13)
                substring/shared
                string-index
                string-index-right
                string-count
                string-concatenate
                string-concatenate-reverse
                string-tokenize
                string-pad
                string-prefix?
                string-suffix?)
         (wak private include)
         (wak fmt))

 (include-file ((wak fmt private) fmt-c))

 )
