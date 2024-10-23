#| License
Copyright (c) 2007-2016 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

#|
(define (procs . imp)
  (let* ((env (apply environment imp)))
    (map car
          (filter (lambda (b) (eq? 'procedure (cdr b)))
                  (environment-bindings env)))))
|#

(library
  (ironscheme constant-fold)
  (export constant-fold)
  (import
    (ironscheme clr)
    (only (rnrs) define for-each)
    (only (rnrs) + - * / = < > <= >= negative? positive? zero? exp expt div mod div0 mod0 even? odd? 
                  log abs tan sin cos atan asin acos angle denominator)
    (except (rnrs arithmetic fixnums) fx*/carry fx-/carry fx+/carry fxdiv0-and-mod0 fxdiv-and-mod)
    (except (rnrs arithmetic flonums) fldiv0-and-mod0 fldiv-and-mod 
              make-no-infinities-violation make-no-nans-violation 
              no-infinities-violation? no-nans-violation?)
    (rnrs arithmetic bitwise))

  (define (set-allow-constant-fold! proc)
    (clr-prop-set! IronScheme.Runtime.Callable AllowConstantFold proc #t))
    
  (define (enable-constant-fold . procs)
    (for-each set-allow-constant-fold! procs))

  (define (constant-fold)
    (enable-constant-fold
      fl+
      fl>=?
      >=
      fl>?
      bitwise-arithmetic-shift
      fl<=?
      >
      fl=?
      <=
      bitwise-reverse-bit-field
      bitwise-rotate-bit-field
      denominator
      zero?
      bitwise-copy-bit-field
      bitwise-bit-field
      mod0
      bitwise-copy-bit
      mod
      bitwise-length
      abs
      div
      div0
      greatest-fixnum
      <
      negative?
      /
      positive?
      *
      =
      -
      +
      fxreverse-bit-field
      fxrotate-bit-field
      fxarithmetic-shift-right
      fxarithmetic-shift-left
      fxcopy-bit-field
      fxbit-field
      fxcopy-bit
      fxif
      real->flonum
      fxmin
      fxmax
      fxodd?
      fxeven?
      fixnum->flonum
      fxzero?
      fxnegative?
      fxpositive?
      bitwise-xor
      fxmod0
      fxdiv0
      fxmod
      fxnot
      fxdiv
      fxxor
      fxior
      fxand
      fx>=?
      bitwise-not
      fx>?
      bitwise-ior
      fx<=?
      bitwise-bit-count
      fx<?
      bitwise-if
      fx=?
      fixnum?
      fixnum-width
      least-fixnum
      fxbit-set?
      flonum?
      fxfirst-bit-set
      flfloor
      fxlength
      flceiling
      fxbit-count
      fl*
      fxarithmetic-shift
      fx-
      fx*
      fx+
      flacos
      fl<?
      bitwise-first-bit-set
      bitwise-arithmetic-shift-left
      flmin
      flmax
      bitwise-bit-set?
      flodd?
      fleven?
      even?
      flzero?
      flnegative?
      flpositive?
      flabs
      bitwise-and
      flround
      fltruncate
      expt
      fldenominator
      flexpt
      flexp
      flsqrt
      fllog
      flatan
      odd?
      flasin
      fltan
      flcos
      flsin
      flnan?
      flinfinite?
      log
      flfinite?
      flinteger?
      tan
      fldiv
      acos
      bitwise-arithmetic-shift-right
      flmod0
      asin
      flmod
      exp
      fldiv0
      fl/
      fl-
      flnumerator
      atan
      angle
      sin
      cos)))
