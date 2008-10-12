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
    (ironscheme core)
    (except (rnrs)
      find
      partition
      filter
      for-all 
      exists
      cons*
      fold-left
      fold-right
      remove
      remv
      remq
      remp
      assq
      assv
      assp
      assoc
      memq
      memv
      member
      memp))
      
  (define (find proc l)
    (if (null? l)
      #f
      (let ((e (car l))
            (r (cdr l))
            (proc proc))
        (if (proc e) 
          e
          (find proc r)))))    
          
  (define (partition proc l)
    (let f ((l l)(a '())(b '()))
      (if (null? l)
        (values (reverse! a) (reverse! b))
        (let ((e (car l)))
          (if (proc e)
            (f (cdr l) (cons e a) b)
            (f (cdr l) a (cons e b)))))))
      
  (define (remp proc l)
    (let f ((l l)(a '()))
      (if (null? l)
        (reverse! a)
        (let ((e (car l)))
          (if (proc e)
            (f (cdr l) a)
            (f (cdr l) (cons e a)))))))
            
  (define (filter proc l)
    (let f ((l l)(a '()))
      (if (null? l)
        (reverse! a)
        (let ((e (car l)))
          (if (proc e)
            (f (cdr l) (cons e a))
            (f (cdr l) a))))))            
  
  (define (assq obj lst)
    (if (null? lst) 
      #f
      (let ((obj obj)
            (t (car lst)) 
            (r (cdr lst)))
        (if (eq? obj (car t)) 
          t
          (assq obj r)))))
          
  (define (assv obj lst)
    (if (null? lst) 
      #f
      (let ((obj obj)
            (t (car lst)) 
            (r (cdr lst)))
        (if (eqv? obj (car t)) 
          t
          (assv obj r)))))          

  (define (assoc obj lst)
    (if (null? lst) 
      #f
      (let ((obj obj)
            (t (car lst)) 
            (r (cdr lst)))
        (if (equal? obj (car t)) 
          t
          (assoc obj r)))))          
            
  (define (assp p? lst)
    (if (null? lst) 
      #f
      (let ((p? p?)
            (t (car lst)) 
            (r (cdr lst)))
        (if (p? (car t)) 
          t
          (assp p? r)))))
  
  (define (memq obj lst)
    (if (null? lst) 
      #f
      (if (eq? obj (car lst)) 
        lst
        (memq obj (cdr lst)))))

  (define (memv obj lst)
    (if (null? lst) 
      #f
      (if (eqv? obj (car lst)) 
        lst
        (memv obj (cdr lst)))))

  (define (member obj lst)
    (if (null? lst) 
      #f
      (if (equal? obj (car lst)) 
        lst
        (member obj (cdr lst)))))

  (define (memp p? lst)
    (if (null? lst) 
      #f
      (if (p? (car lst)) 
        lst
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
              (if (all-empty? cdrs)
                (apply f cars)
                (and (apply f cars) 
                     (apply for-all f cdrs))))))))

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

