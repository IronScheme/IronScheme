(import 
  (rnrs) 
  (except (ironscheme) delay)
  (ironscheme strings)
  (ironscheme regex))

(define-syntax delay*
  (syntax-rules ()
    [(_ ex var)
      (let ((c1 #f)(c2 #f)(r #f))
        (lambda ()
          (when (and c1 (not c2))
            (assertion-violation 'delay* "recursive binding" var))
          (if (and c1 c2)
            r
            (begin
              (set! c1 #t)
              (set! r ex)
              (set! c2 #t)
              r))))]))

(define-syntax lazy-let
  (lambda (x)
    (syntax-case x ()
      [(_ [(var ex) ...] body body* ...) 
        (for-all identifier? #'(var ...))
        #'(let ()
            (let-syntax ((var (identifier-syntax (var))) ...)
              (define var (delay* ex 'var)) ...
              body body* ...))])))
              
        
#|
(define-syntax lazy-let
  (lambda (x)
    (syntax-case x ()
      [(_ [(var ex) ...] body body* ...) 
        (with-syntax (((v ...) (generate-temporaries #'(var ...))))
          #'(let ()
              (let-syntax ((var (identifier-syntax (force v))) ...)
                (define v (delay ex)) ...
                body body* ...)))])))      

(define-syntax lazy-let
  (syntax-rules ()
    [(_ [(var ex) ...] body body* ...) 
      (let ()
          (define x (delay ex)) ...
          (let-syntax ((var (identifier-syntax (force x))) ...)
           body body* ...))]))
           
(define-syntax lazy-let (syntax-rules () [(_ [(var ex) ...] body body* ...) (let () (let-syntax ((var (identifier-syntax (force var))) ...) (define var (delay ex)) ... body body* ...))]))
|#              
(trace-define-syntax grid
  (lambda (x)
    (define (find-ranges vars exps)
      (define (get-coord id)
        (let ((m (regex-match (symbol->string id) "(?<col>[a-zA-Z]+)(?<row>\\d+)?")))
          (let ((col (match-group m "col"))
                (row (match-group m "row")))
            (cons (match-value col)
              (let ((r (match-value row)))
                (and r (string->number r)))))))
      (define (add1 c)
        (integer->char (+ (char->integer c) 1)))
      (define (next-id id)
        (let ((s (reverse (string->list (symbol->string id)))))
          (string->symbol (list->string (reverse (cons (add1 (car s)) (cdr s)))))))
      (define (var? id)
        (exists 
          (lambda (v) 
            (and (eq? (syntax->datum v) id) v))
          vars))
      (define (get-var id)
        (or
          (exists 
            (lambda (v) 
              (and (eq? (syntax->datum v) id) v))
            vars)         
          (assertion-violation 'grid "not a valid variable" id)))
      (define (range? id)
        (and (identifier? id)
          (let ((s (syntax->datum id)))
            (and (symbol? s)
              (let ((t (string-split (symbol->string s) ":")))
                (and (= (vector-length t) 2)
                  (let ((id1 (string->symbol (vector-ref t 0)))
                        (id2 (string->symbol (vector-ref t 1))))
                    (and (var? id1) (var? id2)))))))))
      ;(define (get-range-ids from to)
        ;(let ((from (get-coord from))
              ;(to   (get-coord to)))
          ;(cond
            ;[(
      (define (range-ids id)
        (let ((s (syntax->datum id)))
          (let ((t (string-split (symbol->string s) ":")))
            (and (= (vector-length t) 2)
              (let ((id1 (string->symbol (vector-ref t 0)))
                    (id2 (string->symbol (vector-ref t 1))))
                (printf "~s\n" (get-coord id1))
                (map get-var
                  (let f ((id id1)(a (list id1)))
                    (let ((n (next-id id)))
                      (if (eq? n id2)
                        (reverse (cons n a))
                        (f n (cons n a)))))))))))
      (let ((a '()))
        (define (get-ranges ex)
          (syntax-case ex ()
            [(e ...) (map get-ranges #'(e ...))]
            [id
              (range? #'id)
              (unless (assp 
                        (lambda (e) 
                          (eq? (syntax->datum e) 
                               (syntax->datum #'id))) 
                        a) 
                (set! a (cons (cons #'id (range-ids #'id)) a)))]
            [_ ex]))
        (map get-ranges exps)
        (map
          (lambda (r)
            (with-syntax ((ri (car r))
                          ((rv ...) (cdr r)))
              #'(ri (list rv ...))))
          a)))              
    (syntax-case x ()
      [(_ (var ex) ...)
        (with-syntax ((res #'(list (cons 'var var) ...))
                      (((r rv) ...) (find-ranges #'(var ...) #'(ex ...))))
        #'(let ()
            (let-syntax ((var (identifier-syntax (var))) ...)
              (let-syntax ((r (identifier-syntax rv)) ...)
                (define var (delay* ex 'var)) ... 
                res))))])))
              
(define (prl e)
  (write e)
  (newline))              

;(define v      
  ;(lazy-let ((c (+ a b (let ((a (+ b a))) (+ a b))))
             ;(a (+ 5 (- (* b 3) 1)))
             ;(b 10))
    ;(prl c)                
    ;(list a b c)))
    
(define (sum range)
  (apply + range))    

(define g 
  (grid
    (c (+ a b (let ((c (+ b a))) (+ (sum a:b) c))))
    (a (+ 5 (- (* b 3) 1)))
    (b 10)
    (err (lambda (x) (+ x b)))
    (d (sum a:c))
    (g (let ((f1 1)) (sum (cons f1 f1:f3))))
    (h (sum (map err a:d)))
    (f1 10)
    (f2 5)
    (f3 20)))

;(prl v)  

(prl g)




    