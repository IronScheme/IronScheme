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
    
  (clr-clear-usings)
)