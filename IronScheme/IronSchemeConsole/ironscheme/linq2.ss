(library (ironscheme linq2)
  (export
    ; iterator procs
    iterator?
    make-iterator
    get-iterator
    iterator->list
    iterator->vector
    iterator->string
    iterator->hashtable
    (rename 
      (move-next iterator-move-next)
      (current iterator-current)
      (reset iterator-reset))
    ; LINQ accesor procs
    single
    single/default
    first
    first/default
    last
    last/default
    element-at
    element-at/default
    count
    range
    aggregate
    concat
    any?
    all?
    contains?
    empty
    repeat
    average
    maximum
    minimum
    sum
    take
    skip
    iterator=?
    distinct
    union
    except
    intersect
    ; grouping
    grouping?
    key       ; gets the key of a grouping
    ; main macro's
    from
    foreach   ; C# like syntactic sugar, supports 'break/continue' 
    ; auxiliary keywords
    in
    select
    orderby
    where
    join
    group
    by
    equals
    on
    ascending
    descending
    let
    =
    then
    )

  (import 
    (rnrs))
    
 (define-syntax define-aux
    (syntax-rules ()
      [(_ id ...)
        (begin
          (define-syntax id
            (lambda (x)
              (syntax-violation #f "invalid use of auxiliary keyword" x 'id))) 
          ...)]))
            
  (define-aux 
    select
    in
    orderby
    where
    join
    group
    by
    equals
    on
    ascending
    descending
    then)    

  (define-record-type grouping (fields key iter))
  
  (define key grouping-key)

  (define (get-eq a)
    (cond
      [(or (symbol? a) (boolean? a)) eq?]
      [(string? a) string=?]
      [(number? a) =]
      [(char? a)   char=?]
      [else        eqv?]))

  (define (get-hashtable e)
    ;(make-hashtable equal-hash (get-eq e)))
    (make-eqv-hashtable)) ; for ikarus

  (define (symbol<? a b)
    (string<? 
      (symbol->string a) 
      (symbol->string b)))

  (define (symbol>? a b)
    (string>? 
      (symbol->string a) 
      (symbol->string b)))

  (define (get-comparer asc? a)
    (cond
      [(number? a) (if asc? < >)]
      [(char? a)   (if asc? char<? char>?)]
      [(string? a) (if asc? string<? string>?)]
      [(symbol? a) (if asc? symbol<? symbol>?)]
      [else
        (assertion-violation 'get-comparer "not supported" a)]))

  (define-record-type sorter
    (fields
      asc?
      sel
      (mutable cmp)
      (mutable eq))
    (protocol
      (lambda (p)
        (lambda (asc? sel)
          (p asc? sel #f #f)))))

  (define (sort-iterator iter . sorters)
    (let ((l (iterator->list iter)))
      (if (null? l) 
        (empty)
        (let ((r (cdr l)))
          (if (null? r) 
            (list-iterator l)
            (list-iterator 
              (list-sort
                (lambda (a b)
                  (let f ((sorters sorters))
                    (if (null? sorters) 
                      #f
                      (let* ((s (car sorters))
                             (sel (sorter-sel s))
                             (a (sel a))
                             (b (sel b)))
                        (unless (sorter-eq s)
                          (sorter-eq-set! s (get-eq a))
                          (sorter-cmp-set! s 
                            (get-comparer (sorter-asc? s) a)))
                        (if ((sorter-eq s) a b)
                          (f (cdr sorters))
                          ((sorter-cmp s) a b))))))
              l)))))))

  (define (group-by-iterator iter sel proc)
    (let ((l (iterator->list iter)))
      (if (null? l) 
        (empty)
        (let ((ht (get-hashtable (car l))))
          (for-each
            (lambda (x)
              (let ((key (proc x)))
                (hashtable-update! ht key
                  (lambda (o)
                    (cons (sel x) o))
                  '())))
            l)
          (vector-iterator
            (vector-map
              (lambda (key)
                (make-grouping key (list-iterator (reverse (hashtable-ref ht key '())))))
              (hashtable-keys ht)))))))

  (define (empty-iterator? iter)
    (reset iter) ; needed?
    (let ((r (not (move-next iter))))
      (reset iter)
      r))
              
  ; this is a bit daft, but conforms with the output on LINQ in .NET
  (define (group-if-needed key vals)
    (if (empty-iterator? vals) 
      (empty)
      (make-grouping key vals)))

  (define-syntax bind*
    (syntax-rules ()
      [(bind* (var) K body)
        (lambda (var)
          (let ((K (list var)))
            body))]
      [(bind* (vars ...) K body)
        (lambda (K)
          (apply
            (lambda (vars ...) body)
            K))]))
            
  (define-syntax bind
    (syntax-rules ()
      [(bind (var) body)
        (bind* (var) K body)]
      [(bind (vars ...) body)
        (bind* (vars ...) K body)]))

  (define-syntax from
    (lambda (x)
      (define (valid-id? e vars)
        (and (identifier? e)
          (not (memp
                (lambda (x)
                  (bound-identifier=? e x))
                vars))))
      (syntax-case x (in)
        [(_ e in l rest ...)
          (identifier? #'e)
          (let recur ((vars (list #'e))
                      (l #'(get-iterator l))
                      (x* #'(rest ...)))
            (syntax-case x* (where let = in select from orderby
                             join equals on group by into)
              [() (syntax-violation 'from "missing select or group by clause" x)]
              [(select s into k rest ...)
                (identifier? #'k)
                #`(let ((k (map-iterator #,l (bind #,vars s))))
                  #,(recur (list #'k) #'k #'(rest ...)))]
              [(select s)
                #`(map-iterator #,l (bind #,vars s))]
              [(group g by b into k rest ...)
                (identifier? #'k)
                #`(let ((k (group-by-iterator #,l (bind #,vars g) (bind #,vars b))))
                  #,(recur (list #'k) #'k #'(rest ...)))]
              [(group g by b)
                #`(group-by-iterator #,l (bind #,vars g) (bind #,vars b))]
              [(where p rest ...)
                (recur vars
                  #`(filter-iterator #,l (bind #,vars p))
                  #'(rest ...))]
              [(from e* in l* rest ...)
                (if (valid-id? #'e* vars)
                  (recur (cons #'e* vars)
                    #`(flatten-iterator
                        (map-iterator #,l
                          (bind* #,vars K
                            (map-iterator (get-iterator l*) (lambda (K*) (cons K* K))))))
                     #'(rest ...))
                  (syntax-violation 'from "not a unique identifier" #'e* x*))]
              [(let a = b rest ...)
                (if (valid-id? #'a vars)
                  (recur (cons #'a vars)
                    #`(map-iterator #,l (bind* #,vars K (cons b K)))
                    #'(rest ...))
                  (syntax-violation 'from "not a unique identifier" #'a x*))]
              [(orderby rest ...)
                (with-syntax ((asc #'ascending)
                              (desc #'descending))
                  (let f ((x #'(rest ...))
                          (se '()))
                    (syntax-case x (then)
                      [(p dir then rest ...)
                        (or (free-identifier=? #'dir #'asc)
                            (free-identifier=? #'dir #'desc))
                        (f #'(rest ...)
                          (cons
                            #`(make-sorter
                                #,(free-identifier=? #'dir #'asc)
                                (bind #,vars p))
                             se))]
                      [(p dir rest ...)
                        (or (free-identifier=? #'dir #'asc)
                            (free-identifier=? #'dir #'desc))
                        (recur vars
                          #`(sort-iterator #,l
                              #,@(reverse se)
                              (make-sorter
                                #,(free-identifier=? #'dir #'asc)
                                (bind #,vars p)))
                          #'(rest ...))]
                      [(p rest ...)
                        (f #'(p asc rest ...) se)])))]
              [(join e* in l* on a equals b into f rest ...)
                (if (valid-id? #'e* vars)
                  (if (valid-id? #'f* vars)
                    (with-syntax (((a* eq) (generate-temporaries '(a* eq))))
                      (recur vars l
                         #'(let a* = a
                            let eq = (get-eq a*)
                            let f = (group-if-needed a*
                             (from e* in l*
                              where (eq a* b)
                              select e*)) rest ...)))
                    (syntax-violation 'from "not a unique identifier" #'f x*))
                  (syntax-violation 'from "not a unique identifier" #'e* x*))]
              [(join e* in l* on a equals b rest ...)
                (if (valid-id? #'e* vars)
                  (with-syntax (((a* eq) (generate-temporaries '(a* eq))))
                    (recur vars l
                      #'(let a* = a
                         let eq = (get-eq a*)
                         from e* in l* 
                         where (eq a* b) 
                         rest ...)))
                  (syntax-violation 'from "not a unique identifier" #'e* x*))]
              ))])))
  
  (define-record-type iterator (opaque #t) (fields move-next current reset))

  (define (move-next iter)
    (assert (iterator? iter))
    ((iterator-move-next iter)))
    
  (define (current iter)
    (assert (iterator? iter))
    ((iterator-current iter)))
    
  (define (reset iter)
    (assert (iterator? iter))
    ((iterator-reset iter)))
       
  (define (list-iterator lst)
    (let* ((init (list #f))
           (cur (cons init lst)))
      (make-iterator
        (lambda () 
          (when (null? cur)
            (assertion-violation 'move-next "moved passed end of iterator" lst))
          (set! cur (cdr cur))
          (not (null? cur)))
        (lambda ()
          (when (null? cur)
            (assertion-violation 'current "moved passed end of iterator" lst))
          (when (eq? init (car cur))
            (assertion-violation 'current "move-next not called" lst))                    
          (car cur))
        (lambda()
          (set! cur (cons init lst))))))
          
  (define (vector-iterator vec)
    (let ((index -1)
          (vl (vector-length vec)))
      (make-iterator
        (lambda () 
          (unless (< index vl)
            (assertion-violation 'move-next "moved passed end of iterator" vec index))
          (set! index (+ index 1))
          (< index vl))
        (lambda ()
          (unless (< index vl)
            (assertion-violation 'current "moved passed end of iterator" vec index))
          (when (< index 0)
            (assertion-violation 'current "move-next not called" vec index))          
          (vector-ref vec index))
        (lambda()
          (set! index -1)))))    
          
  (define (string-iterator str)
    (let ((index -1)
          (sl (string-length str)))
      (make-iterator
        (lambda () 
          (unless (< index sl)
            (assertion-violation 'move-next "moved passed end of iterator" str index))
          (set! index (+ index 1))
          (< index sl))
        (lambda ()
          (unless (< index sl)
            (assertion-violation 'current "moved passed end of iterator" str index))
          (when (< index 0)
            (assertion-violation 'current "move-next not called" str index))          
          (string-ref str index))
        (lambda()
          (set! index -1)))))  
          
  (define hashtable-iterator
    (case-lambda
      [(ht)
        (hashtable-iterator ht cons)]
      [(ht sel)
        (define (ht->alist-vec ht)
          (let-values (((k v) (hashtable-entries ht)))
            (vector-map sel k v)))
        (let ((iter #f))
          (make-iterator
            (lambda ()
              (unless iter
                (set! iter (vector-iterator (ht->alist-vec ht))))
              (move-next iter))
            (lambda ()
              (unless iter
                (assertion-violation 'current "move-next not called" iter))
              (current iter))
            (lambda ()
              (set! iter #f))))]))
              
  (define empty
    (let ((ei (make-iterator
                (lambda () #f)
                (lambda () (assertion-violation 'current "not valid"))
                values)))
      (lambda ()
        ei)))
          
  (define (map-iterator iter proc)
    (let* ((init (list #f))
           (end (list #t))
           (cur init))
      (make-iterator
        (lambda ()
          (if (move-next iter)
            (begin
              (set! cur (proc (current iter)))
              #t)
            (begin
              (set! cur end)
              #f)))
        (lambda ()
          (when (eq? cur init)
            (assertion-violation 'current "move-next not called" iter))
          (when (eq? cur end)
            (assertion-violation 'current "moved passed end of iterator" iter))
          cur)
        (lambda ()
          (set! cur init)
          (reset iter)))))
        
  (define (filter-iterator iter proc)
    (make-iterator
      (lambda ()
        (let f ((r (move-next iter)))
          (and 
            r
            (or 
              (proc (current iter))
              (f (move-next iter))))))
      (lambda ()
        (current iter))
      (lambda ()
        (reset iter))))
        
  (define (flatten-iterator iter)
    (let ((outer iter))
      (make-iterator
        (lambda ()
          (define (next)
            (if (eq? iter outer)
              (and (move-next outer)
                (begin
                  (set! iter (current iter))
                  (reset iter)
                  (next)))
              (or 
                (move-next iter)
                (begin
                  (set! iter outer)
                  (next)))))
          (next))  
        (lambda ()
          (current iter))
        (lambda ()
          (set! iter outer)
          (reset iter)))))
          
  (define (reverse-iterator iter)
    (let ((reversed #f))
      (make-iterator
        (lambda ()
          (unless reversed
            (set! iter (list-iterator (reverse (iterator->list iter))))
            (set! reversed #t))
          (move-next iter))
        (lambda ()
          (current iter))
        (lambda ()
          (reset iter)))))  
          
  (define (take-iterator iter count)
    (let ((i -1))
      (make-iterator
        (lambda ()
          (set! i (+ i 1))
          (cond 
            [(< i count) (move-next iter)]
            [else #f]))
        (lambda ()
          (unless (< i count)
            (assertion-violation 'current "moved passed end of iterator" iter i))
          (current iter))
        (lambda ()
          (set! i -1)
          (reset iter)))))
        
  (define (skip-iterator iter count)
    (let ((i 0))
      (make-iterator
        (lambda ()
          (if (< i count)
            (let f ((r (move-next iter)))
              (and r
                (or (= i count)
                  (begin
                    (set! i (+ i 1))
                    (f (move-next iter))))))
            (move-next iter)))
        (lambda ()
          (current iter))
        (lambda ()
          (set! i 0)
          (reset iter)))))
          
  (define (aggregate iter init proc)
    (let ((iter (get-iterator iter)))
      (reset iter)
      (let f ((r (move-next iter))(a init))
        (if r
          (let ((cur (current iter)))
            (f (move-next iter) (proc a cur)))
          a))))
          
  (define (iterator->list iter)
    (reset iter)
    (let f ((r (move-next iter))(a '()))
      (if r
        (let ((cur (current iter)))
          (f (move-next iter) (cons cur a)))
        (reverse a))))
        
  (define (iterator->vector iter)
    (list->vector (iterator->list iter)))
    
  (define (iterator->string iter)
    (list->string (iterator->list iter)))
    
  (define (iterator->hashtable iter sel)
    (reset iter)
    (let ((ht #f))
      (let f ((r (move-next iter)))
        (if r
          (let-values (((k v) (sel (current iter))))
            (unless ht
              (set! ht (get-hashtable k)))
            (hashtable-set! ht k v)
            (f (move-next iter)))
          (or 
            ht
            (make-eq-hashtable))))))    
    
  (define (get-iterator obj)
    (cond
      [(iterator? obj)  obj]
      [(grouping? obj)  (grouping-iter obj)]
      [(list? obj)      (list-iterator obj)]
      [(vector? obj)    (vector-iterator obj)]
      [(string? obj)    (string-iterator obj)]
      [(hashtable? obj) (hashtable-iterator obj)]
      [else
        (assertion-violation 'get-iterator "not supported" obj)]))
        
  (define-syntax foreach
    (lambda (x)
      (syntax-case x (in)
        [(_ a in iterable body body* ...)
          (identifier? #'a)
          (with-syntax
            ([break    (datum->syntax #'a 'break)]
             [continue (datum->syntax #'a 'continue)])
              #'(call/cc
                  (lambda (break)
                    (let ((iter (get-iterator iterable)))
                      (reset iter)
                      (let f ((r (move-next iter)))
                        (when r
                          (let ((a (current iter)))
                            (call/cc
                              (lambda (continue)
                                body body* ...))
                            (f (move-next iter)))))))))])))
                            

  
  (define (single iter)
    (let ((iter (get-iterator iter)))
      (reset iter)
      (if (move-next iter)
        (let ((r (current iter)))
          (if (move-next iter)
            (assertion-violation 'single "contains more than one element" iter)
            r))
        (assertion-violation 'single "contains no elements" iter))))
        
  (define (single/default iter default)
    (let ((iter (get-iterator iter)))
      (reset iter)
      (if (move-next iter)
        (let ((r (current iter)))
          (if (move-next iter)
            (assertion-violation 'single/default "contains more than one element" iter)
            r))
        default)))        

  (define (first iter)
    (let ((iter (get-iterator iter)))
      (reset iter)
      (if (move-next iter)
        (current iter)
        (assertion-violation 'first "contains no elements" iter))))
        
  (define (first/default iter default)
    (let ((iter (get-iterator iter)))
      (reset iter)
      (if (move-next iter)
        (current iter)
        default)))
        
  (define (last iter)
    (let* ((init (list #f))
           (cur init))
      (foreach e in iter
        (set! cur e))
      (if (eq? cur init)
        (assertion-violation 'last "contains no elements" iter)
        cur)))
        
  (define (last/default iter default)
    (let* ((init (list #f))
           (cur init))
      (foreach e in iter
        (set! cur e))
      (if (eq? cur init)
        default
        cur)))         
                                
  (define (count iter)
    (let ((iter (get-iterator iter)))
      (reset iter)
      (let f ((r (move-next iter))(i 0))
        (if r
          (f (move-next iter) (+ i 1))
          i))))
          
  (define range
    (case-lambda
      [(end)
        (range 0 end)]
      [(start end)
        (range start end 1)]
      [(start end skip)
        (let ((cur (- start skip))
              (mnc? #f))
          (make-iterator
            (lambda ()
              (if (< cur end)
                (begin
                  (set! mnc? #t)
                  (set! cur (+ cur skip))
                  (< cur end))
                (assertion-violation 'move-next "moved passed end of iterator")))
            (lambda ()
              (unless mnc?
                (assertion-violation 'current "move-next not called"))
              cur)
            (lambda ()
              (set! cur (- start skip)))))]))
              
              
  (define (concat iter1 iter2)
    (let ((iter1 (get-iterator iter1))
          (iter2 (get-iterator iter2))
          (iter1-done? #f))
      (reset iter1)
      (reset iter2)          
      (make-iterator
        (lambda ()
          (if iter1-done?
            (move-next iter2)
            (let ((r (move-next iter1)))
              (or r
                (begin
                  (set! iter1-done? #t)
                  (move-next iter2))))))
        (lambda ()
          (if iter1-done?
            (current iter2)
            (current iter1)))
        (lambda ()
          (set! iter1-done? #f)
          (reset iter1)
          (reset iter2)))))  
          
  (define (all? iter pred)
    (and 
      (foreach e in iter
        (unless (pred e)
          (break #f)))
      #t))
      
  (define (any? iter pred)
    (not
      (and 
        (foreach e in iter
          (when (pred e)
            (break #f)))
        #t)))
        
  (define contains?
    (case-lambda
      [(iter obj)
        (contains? iter obj (get-eq obj))]
      [(iter obj equals?)
        (any? iter (lambda (e) (equals? e obj)))]))
        
  (define (element-at iter index)
    (let ((i 0)
          (r #f))
      (foreach e in iter
        (when (= i index)
          (set! r e)
          (break)
        (set! i (+ i 1))))
      (if (= i index)
        r
        (assertion-violation 'element-at "index out of range" iter index))))

  (define (element-at/default iter index default)
    (let ((i 0)
          (r #f))
      (foreach e in iter
        (when (= i index)
          (set! r e)
          (break)
        (set! i (+ i 1))))
      (if (= i index)
        r
        default)))   
        
  (define (repeat obj count)
    (let ((i 0))
      (make-iterator
        (lambda ()
          (if (< i count)
            (begin
              (set! i (+ i 1))
              #t)
            #f))
        (lambda ()
          (unless (<= i count)
            (assertion-violation 'current "moved past end of iterator"))
          obj)
        (lambda ()
          (set! i 0)))))
          
  (define (average iter)
    (let ((total 0)
          (count 0))
      (foreach e in iter
        (set! count (+ count 1))
        (set! total (+ total e)))
      (/ total count))) 
      
  (define (maximum iter)
    (let ((iter (get-iterator iter))
          (r -inf.0))
      (when (empty-iterator? iter)
        (assertion-violation 'maximum "empty iterator"))
      (foreach e in iter
        (when (< r e)
          (set! r e)))
      r))  

  (define (minimum iter)
    (let ((iter (get-iterator iter))
          (r +inf.0))
      (when (empty-iterator? iter)
        (assertion-violation 'minimum "empty iterator"))
      (foreach e in iter
        (when (> r e)
          (set! r e)))
      r))
      
  (define (sum iter)
    (aggregate iter 0 +))  
      
  (define (take count iter)                     
    (take-iterator (get-iterator iter) count))
    
  (define (skip count iter)                     
    (skip-iterator (get-iterator iter) count))
    
 
  (define iterator=?
    (case-lambda
      [(iter1 iter2)
        (iterator=? iter1 iter2 (get-eq (first/default iter1 'fake)))]
      [(iter1 iter2 equals?)
        (reset iter1)
        (reset iter2)
        (let f ((r1 (move-next iter1))(r2 (move-next iter2)))
          (if (and r1 r2 (equals? (current iter1) (current iter2)))
            (f (move-next iter1) (move-next iter2))
            (not (or r1 r2))))]))
          
  (define (distinct iter)
    (let ((iter (get-iterator iter))
          (ht #f))
      (reset iter)
      (make-iterator
        (lambda ()
          (unless ht
            (set! ht (get-hashtable (first/default iter 'fake)))
            (reset iter))
          (let f ((r (move-next iter)))
            (if r
              (let ((cur (current iter)))
                (if (hashtable-contains? ht cur)
                  (f (move-next iter))
                  (begin
                    (hashtable-set! ht cur #t)
                    #t)))
              #f)))
        (lambda ()
          (current iter))
        (lambda ()
          (set! ht #f)
          (reset iter)))))
  
  (define (except iter1 iter2)
    (let ((iter1 (get-iterator iter1))
          (iter2 (get-iterator iter2))
          (ht #f))
      (reset iter1)
      (make-iterator
        (lambda ()
          (unless ht
            (set! ht (get-hashtable (first/default iter2 'fake)))
            (foreach e in iter2
              (hashtable-set! ht e #t)))
          (let f ((r (move-next iter1)))
            (if r
              (let ((cur (current iter1)))
                (if (hashtable-contains? ht cur)
                  (f (move-next iter1))
                  (begin
                    (hashtable-set! ht cur #t)
                    #t)))
               #f)))
        (lambda ()
          (current iter1))
        (lambda ()
          (set! ht #f)
          (reset iter1)
          (reset iter2)))))
          
  
  (define (intersect iter1 iter2)
    (let ((iter1 (get-iterator iter1))
          (iter2 (get-iterator iter2))
          (ht #f))
      (reset iter1)
      (make-iterator
        (lambda ()
          (unless ht
            (set! ht (get-hashtable (first/default iter2 'fake)))
            (foreach e in iter2
              (hashtable-set! ht e #t)))
          (let f ((r (move-next iter1)))
            (if r
              (let ((cur (current iter1)))
                (if (hashtable-contains? ht cur)
                  (begin
                    (hashtable-delete! ht cur)
                    #t)
                  (f (move-next iter1))))
               #f)))
        (lambda ()
          (current iter1))
        (lambda ()
          (set! ht #f)
          (reset iter1)
          (reset iter2)))))
   

  (define (union iter1 iter2)
    (distinct (concat iter1 iter2)))
      
)     


;; examples

;; euler 1

#|
(time (sum (from x in (range 0 100000) where (or (zero? (mod x 5)) (zero? (mod x 3))) select x)))

|#  

