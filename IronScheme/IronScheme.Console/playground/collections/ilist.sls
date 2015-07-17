#| License
Copyright (c) 2007-2015 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme collections ilist)
  (export
    ilist?
    ilist-ref
    ilist-set!
    ilist-clear!
    ilist-contains?
    ilist-indexof
    ilist-add!
    ilist-insert!
    ilist-removeat!
    ilist-remove!
    (rename (icollection-count ilist-count)))
  (import 
    (rnrs)
    (ironscheme clr)
    (ironscheme collections icollection))

  (clr-using System.Collections)
  
  (define (ilist? o)
    (clr-is IList o))
  
  (define (ilist-clear! s)
    (clr-call IList Clear s))
  
  (define (ilist-contains? s o)
    (clr-call IList Contains s o))

  (define (ilist-indexof s o)
    (clr-call IList IndexOf s o))
    
  (define (ilist-insert! s n o)
    (clr-call IList Insert s n o))

  (define (ilist-add! s o)
    (clr-call IList Add s o))
  
  (define (ilist-remove! s o)
    (clr-call IList Remove s o))
    
  (define (ilist-removeat! s n)
    (clr-call IList RemoveAt s n))    
    
  (define (ilist-ref s index)
    (clr-indexer-get IList s index))

  (define (ilist-set! s index value)
    (clr-indexer-set! IList s index value))

)