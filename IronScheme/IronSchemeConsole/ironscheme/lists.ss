(library (ironscheme lists (6))
  (export
    find
    
    for-all
    exists
    
    filter
    partition
    
    fold-left
    fold-right
    
    remp
    remove
    remv
    remq
    
    memp
    member
    memv
    memq
    
    assp
    assoc
    assv
    assq
    
    cons*

    )
    
  (import 
    (except (rnrs)
      for-all 
      exists))
  
  (define (all-empty? ls)
    (or (null? ls) 
        (and (null? (car ls)) 
             (all-empty? (cdr ls))))) 
  
  (define (split ls)
    (cond
      ((null? ls) (values '() '()))
      (else 
       (call-with-values (lambda () (split (cdr ls)))
         (lambda (cars cdrs)
           (let ((a (car ls)))
             (values (cons (car a) cars)
                     (cons (cdr a) cdrs))))))))

  (define for-all ;;; almost
    (lambda (f . args)
      (if (all-empty? args) 
          #t
          (call-with-values (lambda () (split args))
            (lambda (cars cdrs)
              (and (apply f cars) 
                   (apply for-all f cdrs)))))))

  (define exists  ;;; almost
    (lambda (f . args)
      (if (all-empty? args) 
          #f
          (call-with-values (lambda () (split args))
            (lambda (cars cdrs)
              (or (apply f cars)
                  (apply exists f cdrs)))))))
)

