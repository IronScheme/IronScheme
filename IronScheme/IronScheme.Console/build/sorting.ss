(library (ironscheme sorting)
  (export
    list-sort
    vector-sort
    vector-sort!)
    
  (import 
    (except (rnrs) list-sort vector-sort)
    (ironscheme clr))
  
  (define (split ls)
    (let loop 
        ((rest ls)
	       (left '())
	       (right '()))
       (cond 
         ((null? rest) (cons left right))
         ((null? (cdr rest)) (cons (cons (car rest) left) right))
         (else (loop 
                  (cddr rest)
                  (cons (car rest) left)
                  (cons (cadr rest) right))))))

  (define (reverse-it head tail)
    (if (null? head)
        tail
        (reverse-it 
          (cdr head) 
          (cons (car head) tail))))
         
  (define (merge list-1 list-2 precedes?)
    (let loop 
        ((source-1 list-1)
         (source-2 list-2)
         (so-far '()))
       (cond 
        ((null? source-1)
          (reverse-it so-far source-2))
        ((null? source-2)
           (reverse-it so-far source-1))
        (else
          (let ((car-1 (car source-1))
                (car-2 (car source-2)))
            (if (precedes? car-2 car-1)
              (loop source-1
                (cdr source-2)
                (cons car-2 so-far))
              (loop source-2
                (cdr source-1)
                (cons car-1 so-far))))))))         

  (define list-sort 
    (lambda (precedes? ls)
     (if (null? ls)
       '()
        (let helper ((piece ls))
         (if (null? (cdr piece))
            piece
            (let ((parts (split piece)))
              (merge (helper (car parts))
                     (helper (cdr parts)) 
                     precedes?)))))))

  (define (vector-sort pred? vec)
    (unless (vector? vec)
      (assertion-violation 'vector-sort "not a vector" vec))
    (let ((vec (clr-call System.Array Clone vec)))
      (vector-sort! pred? vec)
      vec))

                     

)
