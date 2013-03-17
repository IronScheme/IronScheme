#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme syntax parameters)
  (export
    define-syntax-parameter
    syntax-parameterize)
  (import 
    (rename (ironscheme) 
      (define-fluid-syntax define-syntax-parameter)
      (fluid-let-syntax syntax-parameterize))))
