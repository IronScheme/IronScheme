#| License
Copyright (c) 2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme syntax shorthand)
  (export 
    define-syntax-rule
    define-syntax-case)
  (import (ironscheme))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      [(_ (name p ...) b)
        (define-syntax name
          (syntax-rules ()
            [(name p ...) b]))]))

  (define-syntax define-syntax-case
    (syntax-rules ()
      [(_ (name p ...) b)
        (define-syntax name
          (lambda (x)
            (syntax-case x ()
              [(name p ...) b])))])))