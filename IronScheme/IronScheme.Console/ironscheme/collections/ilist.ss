#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

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

  (clr-using system.collections)
  
  (define (ilist? o)
    (clr-is ilist o))
  
  (define (ilist-clear! s)
    (clr-call ilist clear s))
  
  (define (ilist-contains? s o)
    (clr-call ilist contains s o))

  (define (ilist-indexof s o)
    (clr-call ilist indexof s o))
    
  (define (ilist-insert! s n o)
    (clr-call ilist insert s n o))

  (define (ilist-add! s o)
    (clr-call ilist add s o))
  
  (define (ilist-remove! s o)
    (clr-call ilist remove s o))
    
  (define (ilist-removeat! s n)
    (clr-call ilist removeat s n))    
    
  (define (ilist-ref s index)
    (clr-indexer-get ilist s index))

  (define (ilist-set! s index value)
    (clr-indexer-set! ilist s index value))

)