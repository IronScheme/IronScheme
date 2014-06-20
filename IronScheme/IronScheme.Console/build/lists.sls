#| License
Copyright (c) 2007-2014 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

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
    map
    for-each
    make-list
    last-pair
    list-tail
    list-ref
    append
    list?
    length
    list)
  (import 
    (ironscheme core)
    (ironscheme clr)
    (ironscheme unsafe)
    (only (rnrs mutable-pairs) set-cdr!) 
    (ironscheme integrable)
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
      memp
      map
      for-each
      list-tail
      list-ref
      append
      list?
      length
      list))
  
  (define (list . x) x)     
      
  (define (list-tail lst index)
    (cond
      [(zero? index) lst]
      [(or (null? lst) (negative? index))
        (assertion-violation 'list-tail "index out of range" lst index)]
      [else
        (list-tail (cdr lst) (- index 1))]))      
        
  (define (list-ref lst index)
    (car (list-tail lst index)))

  (define (last-pair lst)
    (cond 
      [(null? lst) lst]
      [(null? (cdr lst)) lst]
      [else
        (last-pair (cdr lst))]))

  (define make-list
    (case-lambda
      [(n)      
        (vector->list (make-vector n))]
      [(n fill) 
        (vector->list (make-vector n fill))]))
      
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
  
  (define (split ls who orig-ls)
    (cond
      ((null? ls) (values '() '()))
      (else 
       (call-with-values (lambda () (split (cdr ls) who orig-ls))
         (lambda (cars cdrs)
           (let ((a (car ls)))
             (when (null? a)
               (apply assertion-violation who "Unbalanced lists" orig-ls))
             (values (cons (car a) cars)
                     (cons (cdr a) cdrs))))))))

  ;;http://en.literateprograms.org/Floyd%27s_cycle-finding_algorithm_%28Scheme%29                     
  (define (list/check? head tail)
    (if (pair? head)
        (let ((head ($cdr head)))
           (if (pair? head)
               (and (not (eq? head tail))
                    (list/check? ($cdr head) ($cdr tail)))
               (null? head)))
        (null? head)))
                     
  (define (list? obj)
    (list/check? obj obj))

  (define (length/check head tail i)
    (if (pair? head)
        (let ((head ($cdr head)))
           (if (pair? head)
               (if (not (eq? head tail))
                   (length/check ($cdr head) ($cdr tail) ($fx+ i 2))
                   (assertion-violation 'length "not a proper list"))
               (if (null? head)
                   ($fx+ i 1)
                   (assertion-violation 'length "not a proper list"))))
        (if (null? head)
            i
            (assertion-violation 'length "not a proper list" head))))
          
  (define (length lst)
    (unless (or (null? lst) (pair? lst))
      (assertion-violation 'length "not a pair" lst))
    (length/check lst lst 0))

  (define for-all 
    (case-lambda 
      [(f arg1)
        (if (null? arg1)
            #t
            (if (null? (cdr arg1))
                (f (car arg1))
                (and (f (car arg1)) 
                   (for-all f (cdr arg1)))))]
      [(f arg1 . args)
        (let ((orig-ls (cons arg1 args)))
          (let for-all ((args orig-ls))
            (if (all-empty? args) 
                #t
                (call-with-values (lambda () (split args 'for-all orig-ls))
                  (lambda (cars cdrs)
                    (if (all-empty? cdrs)
                        (apply f cars)
                        (and (apply f cars)
                             (for-all cdrs))))))))]))

  (define exists 
    (case-lambda 
      [(f arg1)
        (if (null? arg1)
            #f
            (or (f (car arg1)) 
                   (exists f (cdr arg1))))]
      [(f arg1 . args)
        (let ((orig-ls (cons arg1 args)))
          (let exists ((args orig-ls))
            (if (all-empty? args) 
                #f
                (call-with-values (lambda () (split args 'exists orig-ls))
                  (lambda (cars cdrs)
                    (or (apply f cars)
                        (exists cdrs)))))))]))

  (define map
    (case-lambda 
      [(proc list1)
        (let f ((lst list1)(a '()))
          (if (null? lst)
              (reverse! a)
              (f (cdr lst) (cons (proc (car lst)) a))))]
      [(proc list1 . lists)
        (let ((orig-ls (cons list1 lists)))
          (let f ((lists orig-ls)(a '()))
            (if (all-empty? lists)
                (reverse! a)
                (call-with-values (lambda () (split lists 'map orig-ls))
                  (lambda (cars cdrs)
                    (f cdrs (cons (apply proc cars) a)))))))]))
  
  (define for-each
    (case-lambda 
      [(f arg1)
        (unless (null? arg1)
          (f (car arg1))
          (for-each f (cdr arg1)))]
      [(f arg1 . args)
        (let ((orig-ls (cons arg1 args)))
          (let for-each ((args orig-ls))
            (if (not (all-empty? args))
                (call-with-values (lambda () (split args 'for-each orig-ls))
                  (lambda (cars cdrs)
                    (apply f cars)
                    (for-each cdrs))))))]))

  (define cons* 
    (lambda (a . rest) 
      (let f ((a a) (rest rest))
        (if (null? rest) 
            a
            (cons a (f (car rest) (cdr rest)))))))
            
  (define (list-copy lst)
    (clr-static-call IronScheme.Runtime.Cons 
                     FromList 
                     lst))

  (define (append-fast! a b)
    (let ((c ($cdr a)))
      (if (null? c)
          (clr-field-set! IronScheme.Runtime.Cons cdr a b)
          (append-fast! c b))))

  (define (append! a b)
    (cond
      [(null? a) b]
      [(null? b) a]
      [else
        (append-fast! a b)
        a]))

  (define append
    (case-lambda
      [()   '()]
      [(obj) obj]
      [(obj1 obj2)
        (unless (list? obj1)
          (assertion-violation 'append "not a list" obj1))
        (append! (list-copy obj1) obj2)]
      [(obj i1 . il)
        (unless (list? obj)
          (assertion-violation 'append "not a list" obj))
        (letrec-syntax ((obj* (identifier-syntax (list-copy obj))))
          (cond 
            [(null? i1)
              (if (null? il)
                  obj*
                  (apply append (cons obj il)))]
            [(pair? i1)
              (if (null? il)
                  (append! obj* i1)
                  (append! obj* (apply append (cons i1 il))))]
            [else
              (append! obj* i1)]))]))

  (define fold-left
    (case-lambda
      [(combine nil lst)
	      (if (null? lst) 
	          nil
	          (fold-left combine 
	                     (combine nil (car lst)) 
	                     (cdr lst)))]
      [(combine nil list1 . lists)
	      (cond 
	        [(null? list1) nil]
	        [else
		        (apply fold-left 
			        (cons*
				        combine 
				        (apply combine (cons* nil (car list1) (map car lists))) 
				        (cdr list1) 
				        (map cdr lists)))])]))
				  
  (define fold-right
    (case-lambda
      [(combine nil lst)
        (if (null? lst)
            nil
	          (combine (car lst) 
	                   (fold-right combine 
	                               nil 
	                               (cdr lst))))]
      [(combine nil list1 . lists)
	      (cond
	        [(null? list1) nil]
		      [else
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
						          (map cdr lists))))))])]))
					    
   (define (remove obj list)
     (remp (lambda (x) (equal? obj x)) list))
    
   (define (remv obj list)
     (remp (lambda (x) (eqv? obj x)) list))
      
   (define (remq obj list)
     (remp (lambda (x) (eq? obj x)) list)))