#| License
Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme collections icollection)
  (export
    icollection?
    icollection-count)
  (import 
    (rnrs)
    (ironscheme clr))

  (clr-using System.Collections)
     
  (define (icollection? o)
    (clr-is ICollection o))

  (define (icollection-count s)
    (clr-prop-get ICollection Count s))

)