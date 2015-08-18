#| License
Copyright (c) 2007-2015 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme typed language)
  (export
    :
    ->
    (rename 
      (lambda: lambda)
      (case-lambda: case-lambda)
      (let: let)
      (let*: let*)
      (define: define)
      (letrec: letrec)
      (letrec*: letrec*)))
  (import 
    (ironscheme typed core)))
