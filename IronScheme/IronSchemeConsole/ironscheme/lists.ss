(library (ironscheme lists)
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
      exists
      cons*
      fold-left
      fold-right
      remove
      remv
      remq
      ;assq
      assv
      assp
      assoc
      ;memq
      ;memv
      member
      memp))
      
  #;(define (assq obj lst)
    (if (null? lst) #f
      (let ((t (car lst)))
        (if (eq? obj (car t)) t
          (assq obj (cdr lst))))))
            
  (define (assv obj lst)
    (if (null? lst) #f
      (let ((c (car lst)))
        (if (eqv? obj (car c)) c
          (assv obj (cdr lst))))))      
            
  (define (assoc obj lst)
    (if (null? lst) #f
      (let ((c (car lst)))
        (if (equal? obj (car c)) c
          (assoc obj (cdr lst))))))
            
  (define (assp p? lst)
    (if (null? lst) #f
      (let ((c (car lst)))
        (if (p? (car c)) c
          (assp p? (cdr lst))))))
      
  #;(define (memq obj lst)
    (if (null? lst) #f
      (if (eq? obj (car lst)) lst
        (memq obj (cdr lst)))))

  #;(define (memv obj lst)
    (if (null? lst) #f
      (if (eqv? obj (car lst)) lst
        (memv obj (cdr lst)))))

  (define (member obj lst)
    (if (null? lst) #f
      (if (equal? obj (car lst)) lst
        (member obj (cdr lst)))))

  (define (memp p? lst)
    (if (null? lst) #f
      (if (p? (car lst)) lst
        (memp p? (cdr lst)))))
  
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
                  
  (define cons* 
    (lambda (a . rest) 
      (let f ((a a) (rest rest))
        (if (null? rest) 
            a
            (cons a (f (car rest) (cdr rest)))))))
            
  
  (define (fold-left combine nil list1 . lists)
	  (if (null? list1)
		  nil
		  (apply fold-left 
			  (cons*
				  combine 
				  (apply combine (cons* nil (car list1) (map car lists))) 
				  (cdr list1) 
				  (map cdr lists)))))   
				  
				  
  (define (fold-right combine nil list1 . lists)
	  (if (null? list1)
		  nil
		  (apply combine 
			  (append
				  (list (car list1))
				  (map car lists)
				  (list 
				    (apply fold-right
					    (cons*
						    combine
						    nil
						    (cdr list1) 
						    (map cdr lists))))))))
						    
						    
   (define (remove obj list)
     (remp (lambda (x) (equal? obj x)) list))
    
   (define (remv obj list)
     (remp (lambda (x) (eqv? obj x)) list))
      
   (define (remq obj list)
     (remp (lambda (x) (eq? obj x)) list))
)

