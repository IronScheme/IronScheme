#!r6rs
#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme linq)
  (export
    ; iterator procs
    iterator?
    make-iterator
    get-iterator
    trace-iterator
    iterator->list
    iterator->vector
    iterator->string
    iterator->hashtable
    (rename 
      (move-next iterator-move-next)
      (current iterator-current)
      (reset iterator-reset))
    ; LINQ accesor procs
    select
    select-many
    where
    reverse-iterator
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
    empty?
    repeat
    average
    maximum
    minimum
    sum
    take
    skip
    take/while
    skip/while
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
    )

  (import 
    (except (rnrs) syntax-case)
    (only (ironscheme core) eqv-hash reverse!)
    (rename (ironscheme syntax symbolic-case) (symbolic-case syntax-case)))

  (define-record-type grouping (fields key iter))

  (define key grouping-key)

  (define (get-eq a)
    (cond
      [(or (symbol? a) 
           (boolean? a)) eq?]
      [(string? a)  string=?]
      [(number? a) =]
      [(char? a)   char=?]
      [else        eqv?]))

  (define (get-hashtable e)
    (make-hashtable eqv-hash (get-eq e)))

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

  (define (sort iter sorters)
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

  (define (order-by-iterator iter . sorters)
    (make-delayed-iterator 
      (lambda ()
        (sort iter sorters))))

  (define (get-group iter sel proc)
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
                  (make-grouping key (list-iterator (reverse! (hashtable-ref ht key '())))))
                (hashtable-keys ht)))))))

  (define (group-by-iterator iter sel proc)
    (make-delayed-iterator 
      (lambda ()
        (get-group iter sel proc))))

  (define (make-delayed-iterator init)
    (let ((iter #f))
      (make-iterator
        (lambda ()
          (unless iter
            (set! iter (init)))
          (move-next iter))
        (lambda ()
          (current iter))
        (lambda ()
          (when iter
            (reset iter))))))

  (define (empty? iter)
    (let ((iter (get-iterator iter)))
      (reset iter) ; needed?
      (let ((r (not (move-next iter))))
        (reset iter) ; or this one? i really need to decide
        r)))

  ; this is a bit daft, but conforms with the output on LINQ in .NET
  (define (group-if-needed key vals)
    (if (empty? vals) 
        (empty)
        (make-grouping key vals)))

  (define (identity x) x)

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
    (lambda (x)
      (syntax-case x ()
        [(bind (var) body)
          (and 
            (identifier? #'body)
            (bound-identifier=? #'var #'body))
          #'identity]
        [(bind (var) body)
          #'(lambda (var) body)]
        [(bind (vars ...) body)
          #'(bind* (vars ...) K body)])))

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
                          #`(order-by-iterator #,l
                              #,@(reverse! se)
                              (make-sorter
                                #,(free-identifier=? #'dir #'asc)
                                (bind #,vars p)))
                          #'(rest ...))]
                      [(p rest ...)
                        (f #'(p asc rest ...) se)])))]
              [(join e* in l* on a equals b into f rest ...)
                (identifier? #'f)
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
                    (syntax-violation 'from "not a unique identifier" #'e* x*))]))])))

  (define-record-type iterator 
    (opaque #t) 
    (fields move-next 
            current 
            reset))

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
      (lambda () ei)))

  (define (map-iterator iter proc)
    (if (eq? proc identity)
      iter
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
            (reset iter))))))
  
  (define (select x proc)
    (map-iterator (get-iterator x) proc))            

  (define (filter-iterator iter proc)
    (make-iterator
      (lambda ()
        (let f ((r (move-next iter)))
          (and r
              (or (proc (current iter))
                  (f (move-next iter))))))
      (lambda ()
        (current iter))
      (lambda ()
        (reset iter))))
        
  (define (where x proc)
    (filter-iterator (get-iterator x) proc))            

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
          
  (define (select-many x proc)
    (flatten-iterator 
      (map-iterator 
        (get-iterator x) 
        (lambda (x) 
          (get-iterator (proc x))))))

  (define (reverse-iterator iter)
    (let ((reversed #f))
      (make-iterator
        (lambda ()
          (unless reversed
            (set! iter (list-iterator (reverse! (iterator->list iter))))
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
          
  (define (take/while-iterator iter pred)
    (make-iterator
      (lambda ()
        (and (move-next iter) (pred (current iter))))
      (lambda ()
        (current iter))
      (lambda ()
        (reset iter))))

  (define (skip/while-iterator iter pred)
    (let ((found #f))
      (make-iterator
        (lambda ()
          (if found
              (move-next iter)
              (let f ((r (move-next iter)))
                (and r
                    (if (pred (current iter))
                        (f (move-next iter))
                        (begin 
                           (set! found #t)
                           #t))))))
        (lambda ()
          (current iter))
        (lambda ()
          (set! found #f)
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
          (reverse! a))))

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
            (or ht (make-eq-hashtable))))))

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
      (when (empty? iter)
        (assertion-violation 'maximum "empty iterator"))
      (foreach e in iter
        (when (< r e)
          (set! r e)))
      r))  

  (define (minimum iter)
    (let ((iter (get-iterator iter))
          (r +inf.0))
      (when (empty? iter)
        (assertion-violation 'minimum "empty iterator"))
      (foreach e in iter
        (when (> r e)
          (set! r e)))
      r))

  (define (sum iter)
    (aggregate iter 0 +))
    
  (define (take count iter)
    (take-iterator  (get-iterator iter) count))

  (define (skip count iter)                     
    (skip-iterator (get-iterator iter) count))    

  (define (take/while pred iter)
    (take/while-iterator (get-iterator iter) pred))

  (define (skip/while pred iter)                     
    (skip/while-iterator (get-iterator iter) pred))

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

  (define (trace-iterator name iter)
    (let ((iter (get-iterator iter)))
      (make-iterator
        (lambda ()
          (move-next iter))
        (lambda ()
          (let ((cur (current iter)))
            (display name)
            (display " -> ")
            (write cur)
            (newline)
            cur))
        (lambda ()
          (reset iter))))))

;; examples

#|
(import (ironscheme linq))

;; euler 1 (bit bigger to test for proper tail calls)

(sum 
  (from x in (range 1000000) 
   where (or (fxzero? (fxmod x 5)) (fxzero? (fxmod x 3))) 
   select x))
   
VS

(let ((x 1000000))
  (let s ((cur-value 1) (sum 0))
    (cond
      ((fx>=? cur-value x) sum)
      ((or (fxzero? (fxmod cur-value 3)) (fxzero? (fxmod cur-value 5)))
        (s (fx+ cur-value 1) (+ sum cur-value)))
      (else (s (fx+ cur-value 1) sum)))))

;; extract proc forms
(let* ((env (environment '(ironscheme linq)))
       (bindings (environment-bindings env)))
  (foreach f in (from eb in bindings
                 let s = (car eb)
                 where (let ((type (cdr eb))) 
                         (eq? 'procedure type))
                 orderby s
                 let b = (eval s env)
                 let forms = (call-with-values 
                               (lambda () (procedure-form b)) 
                               list)
                 from f in (map cdr forms)
                 where (list? f)
                 group (cons s f) by (length f) into b
                 orderby (key b)
                 from f in b
                 select f)
    (printf "~a\n" f)))
 
|# 
;; docs

#|

LINQ for R6RS Scheme
====================

See below code for examples and tests.


The following grammar is as per the C# spec [1]. 

Grammar notes:
- The optional type token is not present and has been removed from the grammar.
- IDENTIFIER is any Scheme identifier.
- Other uppercased or single quoted 'tokens' are translated to lowercased Scheme syntax, with exceptions.
- expression is any Scheme expression.
- boolean_expression is Scheme expression returning a boolean value.
- * denotes 0 or more.
- ? denotes optional.
- Differences in C++ style comments.
- Starts with query_expression.

query_expression
  : from_clause query_body
  ;
  
from_clause
  : FROM IDENTIFIER IN expression
  ;
  
query_body
  : query_body_clause* select_or_group_clause query_continuation?
  ;
  
query_body_clause
  : from_clause
  | let_clause
  | where_clause
  | join_clause
  | join_into_clause
  | orderby_clause
  ;
  
let_clause
  : LET IDENTIFIER '=' expression
  ;
  
where_clause
  : WHERE boolean_expression
  ;
  
join_clause
  : JOIN IDENTIFIER IN expression ON expression EQUALS expression 
  ;
  
join_into_clause
  : JOIN IDENTIFIER IN expression ON expression EQUALS expression INTO IDENTIFIER
  ;
  
orderby_clause
  : ORDERBY orderings
  ;

// ',' is replaced by THEN  
orderings
  : ordering
  | orderings ',' ordering
  ;
  
ordering
  : expression ordering_direction?
  ;

ordering_direction
  : ASCENDING
  | DESCENDING
  ;
  
select_or_group_clause
  : select_clause
  | group_clause
  ;
  
select_clause
  : SELECT expression
  ;
  
group_clause
  : GROUP expression BY expression
  ;

query_continuation
  : INTO IDENTIFIER query_body 
  ;

Iterator design:

http://csharpindepth.com/Articles/Chapter6/IteratorBlockImplementation.aspx
http://csharpindepth.com/Articles/Chapter11/StreamingAndIterators.aspx  

Transform process:

http://bartdesmet.net/blogs/bart/archive/2008/08/30/c-3-0-query-expression-translation-cheat-sheet.aspx


Procedures:

(aggregate iter init proc)

  Aka fold-left. proc takes 2 args (accumalator and element) and returns a single value. 

(all? iter pred)

  Aka for-all. 
  
(any? iter pred)

  Aka exists.
  
(average iter)

  Gets the average of a iterable of numbers.

(concat iter1 iter2)

  Return an iterator for the concatenation of both iterators.
  
(contains? iter obj)

  Tests for the existence of an object in a list.

(contains? iter obj equals?)

  Tests for the existence of an object in a list. equals? is the predicate.
  
(count iter)

  Returns the number of elements in the iterator.
  
(distinct iter)

  Returns an iterator of dictict elements of iter.
  
(element-at iter index)

  Gets the element at a specified index. Asserts when index is out of bounds.
  
(element-at/default iter index default)

  Gets the element at a specified index. Returns default when index is out of bounds.

(empty)

  Returns an empty iterator (singleton).
  
(empty? iter)

  Tests if an iterator is empty (contains no elements).  
  
(except iter1 iter2)

  Returns an iterator that contains the set difference of the elements of two iterators. 
  
(first iter)

  Gets the first element of an iterator. Asserts when iterator is empty.
  
(first/default iter default)

  Gets the first element of an iterator. Returns default when iterator is empty.
  
(get-iterator obj)

  Gets an iterator from an iterable object, eg list, vector, string, etc.
  
(grouping? obj)

  Tests whether an object is a grouping.
  
(intersect iter1 iter2)

  Returns an iterator that contains the elements that form the set intersection of two iterators.

(iterator->hashtable iter sel)

  Converts an iterator to a hashtable. sel accepts 1 value, and returns 2 values for the key and value. The hashtable is based on the first element in the list.
  
(iterator->list iter)

  Converts an iterator to a list. 
  
(iterator->string iter)

  Converts an iterator to a string. All elements must be characters.
  
(iterator->vector iter)

  Converts an iterator to a vector. 
  
(iterator-current iter)

  Gets the current value of an iterator.
  
(iterator-move-next iter)

  Moves an iterator to the next element. Returns #f if no more elements. Must be called before calling iterator-current.
  
(iterator-reset iter)

  Resets an iterator to it's initial state.
  
(iterator=? iter1 iter2)

  Determines whether two iterators are equal by comparing the elements.
  
(iterator=? iter1 iter2 equals?)

  Determines whether two iterators are equal by comparing the elements. Uses equals? for predicate.
  
(iterator? obj)

  Tests if an object is an iterator.
  
(key grouping)

  Gets the key of a grouping.
  
(last iter)

  Gets the last element of an iterator. Asserts when iterator is empty.
  
(last/default iter default)

  Gets the last element of an iterator. Returns default when iterator is empty.
  
(make-iterator move-next current reset)

  Returns a primitive iterator. 

(maximum iter)

  Returns the maximum element of an iterator. Must be number.
  
(minimum iter)

  Returns the minimum element of an iterator. Must be number.
  
(range end)

  Returns an iterator of numbers starting with 0, incrementing 1, up to, but excluding 'end'. 
  
(range start end)

  Returns an iterator of numbers starting with 'start', incrementing 1, up to, but excluding 'end'. 
  
(range start end skip)

  Returns an iterator of numbers starting with 'start', incrementing 'skip', up to, but excluding 'end'. 
  
(repeat obj count)

  Returns an iterator that contains one repeated value, for count number of times.
  
(single iter)

  Gets the first and only element of an iterator. Asserts when iterator is empty or more than 1 element.
    
(single/default iter default)

  Gets the first and only element of an iterator. Returns default when iterator is empty. Asserts when more than 1 element in iterator.
  
(skip count iter)

  Returns an iterator that skips count number of elements of an iterator. Returns empty iterator if count is smaller that the count of the iterator.
  
(sum iter)

  Gets the sum of a iterable of numbers.
  
(take count iter)
  
  Returns an iterator that take count number of elements of an iterator. Returns empty iterator if count is smaller that the count of the iterator.
  
(union iter1 iter2)

  Returns an iterator that contains the elements from both iterators, excluding duplicates. 
  
References:

1. http://download.microsoft.com/download/3/8/8/388e7205-bc10-4226-b2a8-75351c669b09/CSharp%20Language%20Specification.doc , 7.15 Query expressions  

Tests:
======

(import (ironscheme linq))

;; conformance to C# tests
;; simply permutations of the grammar and matched to the output of C#
(define (print-list lst)
  (foreach x in lst
    (printf "~a, " x))
  (printf "\n"))

(define selectdata  '(1 5 3 4 2))
(define groupdata   '(2 5 2 4 2))
(define nestdata    '((2 5)(2 4)(3 5)(3 1)(1 1)))


(define a ( from x in selectdata
            select x))

(print-list a)

(define a2 (from x in (from y in selectdata
                       select (+ y 1))
            select (- x 1)))

(print-list a2)

(define b ( from x in selectdata
            where (even? x)
            select x))

(print-list b)

(define c ( from x in selectdata
            orderby x
            select x))

(print-list c)

(define d ( from x in selectdata
            orderby x descending
            select x))

(print-list d)

(define e ( from x in selectdata
            where (odd? x)
            orderby x
            select x))

(print-list e)

(define f ( from x in selectdata
            let y = (* x x)
            select y))

(print-list f)

(define f2 (from x in selectdata
            let y = (* x x)
            where (odd? y)
            orderby y descending
            select y))

(print-list f2)

(define g ( from x in selectdata
            select x into z
            select z))

(print-list g)

(define h ( from x in nestdata
            from y in x
            select y))

(print-list h)

(define i ( from x in nestdata
            where (= (car x) 2)
            from y in x
            select y))

(print-list i)

(define j ( from x in nestdata
            orderby (car x)
            from y in x
            select y))

(print-list j)

(define k ( from x in nestdata
            from y in x
            orderby y
            select y))

(print-list k)

(define l ( from x in nestdata
            select x into y
            from z in y
            select z))

(print-list l)

(define m ( from x in nestdata
            group (car x) by (cadr x)))

(print-list m)

(define n ( from x in nestdata
            group (cadr x) by (car x)))

(print-list n)

(define o ( from x in nestdata
            group (cadr x) by (car x) into z
            select z))

(print-list o)

(define p ( from x in nestdata
            group (cadr x) by (car x) into z
            orderby (key z) descending
            select z))

(print-list p)

(define q ( from x in selectdata
            join y in groupdata on x equals y into z
            from w in z
            select w))

(print-list q)

(define r ( from x in selectdata
            join y in groupdata on x equals y
            select (cons x y)))

(print-list r)

(define s ( from x in selectdata
            from y in groupdata
            where (and (= x 4) (= y 2))
            select (cons x y)))

(print-list s)

(define t ( from x in selectdata
            join y in groupdata on x equals y into z
            orderby x
            select z))

(print-list t)

(define u ( from x in selectdata
            join y in groupdata on x equals y into z
            orderby x
            select (cons x z)))

(print-list u)

(define v ( from x in selectdata
            from y in groupdata 
            select y into z
            where (even? z)
            select z))

(print-list v)


|#


