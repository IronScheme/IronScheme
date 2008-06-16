(library (ironscheme mutable-pairs)
  (export
    append!
    set-car!
    set-cdr!)
    
  (import 
    (rnrs)
    (rnrs mutable-pairs))
  
  (define (append! a . b)
    (cond
      [(null? a) b]
      [(null? b) a]
      [else
        (let f ((a a))
          (let ((n (cdr a)))
            (if (null? n)
              (set-cdr! a b)
              (f n))))
        a]))
)
