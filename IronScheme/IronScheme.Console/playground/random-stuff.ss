(import (ironscheme clr))
(import (ironscheme clr dynamic))
(clr-dynamic (list 12 34) PrettyPrint)
(clr-dynamic (list 12 34) car 1)
(define int->string (clr-call-site Int32 ToString))
(clr-using System.Reflection.Emit)
(define emit (clr-call-site ILGenerator Emit))
(clr-static-call-site DateTime Parse)


(define-record-type node 
  (protocol (lambda (p)
              (lambda (key)
                (p key #f '()))))
  (fields key 
          (mutable value) 
          (mutable children)))

(define (get-node node key)
  (let f ((c (node-children node)))
    (if (null? c)
        #f
        (let ((node (car c)))
          (if (bound-identifier=? (node-key node) key)
              node
              (f (cdr c)))))))

(define (make-tree)  (make-node #f))

(define (tree-add! tree value keys)
  (let f ((keys keys)(node tree))
    (let ((cnode (or (get-node node (car keys))
                     (let ((cnode (make-node (car keys))))
                       (node-children-set! node (cons cnode (node-children node)))
                       cnode))))
      (if cnode
          (if (null? (cdr keys))
              (node-value-set! cnode value)
              (f (cdr keys) cnode))))))
              
(define (get-child-keys node)
  (map node-key (node-children node)))              
                
(define (generate-node node ids)
  (with-syntax ((pred (node-key node))
                (id   (car ids)))
    (let ((val (node-value node)))
      (if val
          (with-syntax ((val val))
            #'((pred id) val))
          (with-syntax (((c ...) (map (lambda (x)
                                        (generate-node x (cdr ids))) 
                                      (node-children node)))
                        (child-keys (get-child-keys node))                                      
                        (next-id (car (cdr ids))))
            #'((pred id)
                (cond
                  c ...
                  ;;;
                  [else (error #f "not matched" next-id 'child-keys)])))))))

(define (generate-tree tree ids)
  (with-syntax (((c ...) (map (lambda (x)
                                (generate-node x ids)) 
                              (node-children tree)))
                (child-keys (get-child-keys tree))                              
                (next-id (car ids)))                              
    #'(cond
        c ...
        [else (error #f "not matched" next-id 'child-keys)])))

(trace-define-syntax fsm-cond
  (lambda (x)
    (syntax-case x ()
      [(_ (id ...) ((pred ...) expr) ...)
        (for-all identifier? #'(pred ... ... id ...))
        (let* ((tree (make-tree)))
          (for-each (lambda (preds expr)
                      (tree-add! tree expr preds))
                    #'((pred ...) ...)
                    #'(expr ...))
          (generate-tree tree #'(id ...)))])))

(import (ironscheme fsm-cond))
(define test
  (lambda (a b c)
    (fsm-cond (a b c)
      [(fixnum?   boolean?  fixnum?)  'case1]
      [(fixnum?   symbol?   symbol?)  'case2]
      [(fixnum?   symbol?   boolean?) 'case3]
      [(fixnum?   boolean?  boolean?) 'case22]
      [(symbol?   symbol?   boolean?) 'case4]
      [(fixnum?   values  fixnum?)    'case5]
      [else #f])))

(import (ironscheme clr-cond))
(define test
  (lambda (a b c)
    (clr-cond (a b c)
      [(Int32   Int32?   Int32)   'case1]
      [(Int32   String  String)  'case2]
      [(Int32   String  Boolean) 'case3]
      [(Int32   Boolean Boolean) 'case22]
      [(String  String  Boolean) 'case4]
      [(Int32   Object  Int32)   'case5])))
      

(define ($ x)
  (printf "~s\n" x))
   

(lambda (a b c)      
  (cond
    [(fixnum? a)
      (cond
        [(boolean? b) 
          (cond 
            [(symbol? c) 'case1]
            [(fixnum? c) 'case5]
            [else (error #f "not matched" c)])]
        [(symbol? b)
          (cond
            [(symbol? c) 'case2]
            [(boolean? c) 'case3]
            [else (error #f "not matched" c)])]
        [else (error #f "not matched" b)])]
    [(and (symbol? a) (symbol? b) (fixnum? c)) 'case4]
    [else (error #f "not matched" a)]))


(import 
  (ironscheme)
  (ironscheme clr))

(define (make-stopwatch)
  (clr-static-call System.Diagnostics.Stopwatch StartNew))
  
(define (elapsed-milliseconds sw)
  (clr-prop-get System.Timespan TotalMilliseconds 
    (clr-prop-get System.Diagnostics.Stopwatch Elapsed sw)))
    
(define (print expr iters ms)
  (display "Benchmark:  ")
  (write expr)
  (newline)
  (display "Iterations: ")
  (write iters)
  (newline)
  (display "Total ms:   ")
  (write ms)
  (newline)
  (display "Average ms: ")
  (write (/ ms iters))
  (newline)
  (newline))

    
(define-syntax bench
  (syntax-rules ()
    [(_ expr)
      (bench expr 10000000)]
    [(_ expr iters)
      (let ((sw (make-stopwatch)))
        (let loop ((i 0))
          (cond
            [(fx=? i iters)]
            [else
              expr
              (loop (fx+ i 1))]))
        (print 'expr iters (elapsed-milliseconds sw)))]))
        

(bench #t)
(bench #f)
(bench (void))
(bench (+ 1000 1000))
(bench (fx+ 1000 1000))
(bench (fl+ 1000.0 1000.0))
(bench (/ 1000 100))
(bench (fxdiv 1000 100))
(bench (fl/ 1000.0 100.0))

(let ((a 10000)(b 100))
  (bench (+ a b))
  (bench (fx+ a b))
  (bench (- a b))
  (bench (fx- a b))
  (bench (* a b))
  (bench (fx* a b)))


(bench (let ((a 10000)(b 100)) (+ a b)))
(bench (let ((a 10000)(b 100)) (fx+ a b)))
(bench (let ((a 10000)(b 100)) (- a b)))
(bench (let ((a 10000)(b 100)) (fx- a b)))
(bench (let ((a 10000)(b 100)) (* a b)))
(bench (let ((a 10000)(b 100)) (fx* a b)))


(define generate-indices
  (lambda this-list
    (let ((indice-list (cons 0 '())))
      (let loop ((c-elem this-list)
                 (c-indice 1)
                 (c-ip indice-list))
        (if (not (null? c-elem))
          (begin
            (set-cdr! c-ip (cons c-indice '()))
            (loop (cdr c-elem) (+ c-indice 1) (cdr c-ip))))
        indice-list))))
         
(define-syntax generate-structure
  (syntax-rules ()
    ((_ struct-name (mand-field opt-fields ...))
     (begin
       (apply (lambda (mand-field opt-fields ... n-fields)
                (let ((data (make-vector n-fields)))
                  (lambda (command . argument)
                    (case command
                      ((opt-fields)
                        (if (null? argument)
                           (vector-ref data opt-fields)
                           (vector-set! data opt-fields (car argument)))) ...
                      (else (format "No matched field...!\n"))))))
               (generate-indices 'mand-field 'opt-fields ...))))))

(define a (generate-structure test_t (field-1 field-2 field-3)))

(define (make-type-final type)
  type)

(define-syntax make-type
  (lambda (x)
    (syntax-case x ()
      [(_ type)
        (with-syntax ((type (get-clr-type (syntax->datum #'type))))
          #'(make-type-final type))])))

(import (ironscheme threading))

(define (make-threaded-eval id)
  (let ((env (new-interaction-environment)))
    (lambda (expr)
      (queue-work-item 
        (lambda (state)
          (let ((result (eval expr env)))
            (printf "Result on ~a ~s\n" state result)))
        id)
      (void))))

(define eval-1 (make-threaded-eval 'thread1))
(define eval-2 (make-threaded-eval 'thread2))

(eval-1 '(define a 1))
(eval-2 '(define a 99))

(eval-1 'a) ;=> 1
(eval-2 'a) ;=> 99

(eval-1 '(set! a -1))
(eval-2 '(set! a -99))

(eval-1 'a) ;=> -1
(eval-2 'a) ;=> -99




(let ()
  (let* ((a (+ 1 2))
         (b 3)
         (c a)
         (d (- c b)))
    (let* ((a c)
           (b 2)
           (c a)
           (d (- c b)))
      (* a b c d))))
 



(import (ironscheme ffi))   
(define dlload (pinvoke-call kernel32 LoadLibrary intptr (string)))
(define dlsym (pinvoke-call kernel32 GetProcAddress intptr (intptr string)))
(define ffitestlib (dlload "ffitest"))
(define co (dlsym ffitestlib "fnffitest"))
(define cb (dlsym ffitestlib "fnfficallback"))
(define co-sig (ffi-callout int32 (int32 int32)))
(define cb-sig (ffi-callout int32 (intptr)))
(define fnfficallback (cb-sig cb))
(define fxplus ((ffi-callback int32 (int32 int32)) 
  (lambda (x y)
    (printf "I got ~a and ~a\n" x y)
    (fx+ x y))))

(fnfficallback fxplus)

;(define malloc-sig (ffi-callout void* (uint32)))
(define malloc-sig (make-ffi-callout 'void* '(uint32)))
(define lib (dlload "msvcrt"))
(define proc (dlsym lib "malloc"))

(define malloc (malloc-sig proc))

(define mem (malloc 8))



(define ffitestlib (dlload "ffitest.dll"))
(define co (dlsym ffitestlib "fnffitest"))

  
(define make-next-coords
  (lambda (x)
    (with-syntax (((name ... pos inc) x))
      (let ((pos (syntax->datum #'pos)))
        (let f ((names #'(name ...))(pos pos))
          (with-syntax (((name rest ...) names))
            (if (zero? pos)
                #'((+ name inc) rest ...)
                #`(name #,@(f (cdr names)(- pos 1))))))))))
            
(write (syntax->datum (make-next-coords #'(a b c d e 2 -1))))

(begin
  (define foo
    (case-lambda
      ((x y z) (list 'one x))
      ((x y) (list 'two x y))
      ((a . b) (list 'plus2 a b))))
   (foo 1))
 
 
(format "~8,2F" 320000000000.0) => "320000000000.00" ; *** failed ***
 ; expected result: " 3.20e11"   
(format "~8,2F" 345670000000.0) => "345670000000.00" ; *** failed ***
 ; expected result: " 3.46e11" 
(format "~f" -12300000000.0) => "-12300000000.0" ; *** failed ***
 ; expected result: "-1.23e10"  
 
 
 
 (define flonum->digits
  (lambda (v f e min-e p b ob)
    (if (>= e 0)
        (if (not (= f (expt b (- p 1))))
            (let ([be (expt b e)])
              (scale (* f be 2) 2 be be 0 ob #f #f v))
            (let* ([be (expt b e)] [be1 (* be b)])
              (scale (* f be1 2) (* b 2) be1 be 0 ob #f #f v)))
        (if (or (= e min-e) (not (= f (expt b (- p 1)))))
            (scale (* f 2) (* (expt b (- e)) 2) 1 1 0 ob #f #f v)
            (scale (* f b 2) (* (expt b (- 1 e)) 2)
                   b 1 0 ob #f #f v)))))

(define scale
  (lambda (r s m+ m- k ob low-ok? high-ok? v)
    (let ([est (inexact->exact (ceiling (- (logB ob v) 1e-10)))])
      (if (>= est 0)
          (fixup r (* s (exptt ob est)) m+ m- est ob low-ok? high-ok?)
          (let ([scale (exptt ob (- est))])
            (fixup (* r scale) s (* m+ scale) (* m- scale)
                   est ob low-ok? high-ok?))))))

(define fixup
  (lambda (r s m+ m- k ob low-ok? high-ok?)
    (if ((if high-ok? >= >) (+ r m+) s) ; too low?
        (cons (+ k 1) (generate r s m+ m- ob low-ok? high-ok?))
        (cons k
              (generate (* r ob) s (* m+ ob) (* m- ob) ob low-ok? high-ok?)))))

(define generate
  (lambda (r s m+ m- ob low-ok? high-ok?)
    (let ([d (quotient r s)]
          [r (remainder r s)])
      (let ([tc1 ((if low-ok? <= <) r m-)]
            [tc2 ((if high-ok? >= >) (+ r m+) s)])
        (if (not tc1)
            (if (not tc2)
                (cons d (generate (* r ob) s (* m+ ob) (* m- ob)
                                  ob low-ok? high-ok?))
                (list (+ d 1)))
            (if (not tc2)
                (list d)
                (if (< (* r 2) s)
                    (list d)
                    (list (+ d 1)))))))))

(define exptt
  (let ([table (make-vector 326)])
    (do ([k 0 (+ k 1)] [v 1 (* v 10)])
        ((= k 326))
      (vector-set! table k v))
    (lambda (B k)
      (if (and (= B 10) (<= 0 k 325))
          (vector-ref table k)
          (expt B k)))))

(define logB
  (let ([table (make-vector 37)])
    (do ([B 2 (+ B 1)])
        ((= B 37))
      (vector-set! table B (/ (log B))))
    (lambda (B x)
      (if (<= 2 B 36)
          (* (log x) (vector-ref table B))
          (/ (log x) (log B))))))
          
(call-with-values (lambda () (decompose-flonum 1.0)) flonum->digits)







;;; Free-format algorithm for printing IEEE double-precision positive
;;; floating-point numbers in base 10

;;; It uses the floating-point logarithm to estimate the scaling factor
;;; and a table to look up powers of ten.

;;; Input to flonum->digits:
;;;       v -- a positive floating-point number, f x 2^e
;;;       f -- mantissa of v
;;;       e -- exponent of v

;;; Output: (k d_1 d_2 ... d_n),
;;;   where 0.d_1...d_n x 10^k is the shortest correctly rounded base-10
;;;   number that rounds to v when input (it assumes the input
;;;   routine rounds to even)

;;; See also "Printing Floating-Point Numbers Quickly and Accurately"
;;; in Proceedings of the SIGPLAN '96 Conference on Programming Language
;;; Design and Implementation.

;;; Author: Bob Burger  Date: March 1996


(define flonum->digits
  (let ([min-e -1074]
        [bp-1 (expt 2 52)])
    (lambda (v f e)
      (let ([round? (even? f)])
        (if (>= e 0)
            (if (not (= f bp-1))
                (let ([be (expt 2 e)])
                  (scale (* f be 2) 2 be be 0 round? round? v))
                (let ([be (expt 2 e)])
                  (scale (* f be 4) 4 (* be 2) be 0 round? round? v)))
            (if (or (= e min-e) (not (= f bp-1)))
                (scale (* f 2) (expt 2 (- 1 e)) 1 1 0 round? round? v)
                (scale (* f 4) (expt 2 (- 2 e)) 2 1 0 round? round? v)))))))

(define scale
  (lambda (r s m+ m- k low-ok? high-ok? v)
    (let ([est (inexact->exact (ceiling (- (log10 v) 1e-10)))])
      (if (>= est 0)
          (fixup r (* s (expt10 est)) m+ m- est low-ok? high-ok?)
          (let ([scale (expt10 (- est))])
            (fixup (* r scale) s (* m+ scale) (* m- scale)
                   est low-ok? high-ok?))))))

(define fixup
  (lambda (r s m+ m- k low-ok? high-ok?)
    (if ((if high-ok? >= >) (+ r m+) s) ; too low?
        (cons (+ k 1) (generate r s m+ m- low-ok? high-ok?))
        (cons k
              (generate (* r 10) s (* m+ 10) (* m- 10) low-ok? high-ok?)))))

(define generate
  (lambda (r s m+ m- low-ok? high-ok?)
    (let ([d (quotient r s)]
          [r (remainder r s)])
      (let ([tc1 ((if low-ok? <= <) r m-)]
            [tc2 ((if high-ok? >= >) (+ r m+) s)])
        (if (not tc1)
            (if (not tc2)
                (cons d (generate (* r 10) s (* m+ 10) (* m- 10)
                                  low-ok? high-ok?))
                (list (+ d 1)))
            (if (not tc2)
                (list d)
                (if (< (* r 2) s)
                    (list d)
                    (list (+ d 1)))))))))

(define expt10
  (let ([table (make-vector 326)])
    (do ([k 0 (+ k 1)] [v 1 (* v 10)])
        ((= k 326))
      (vector-set! table k v))
    (lambda (k)
      (vector-ref table k))))

(define log10
  (let ([f (/ (log 10))])
    (lambda (x)
      (* (log x) f))))

(define (get-digits flo)
  (call-with-values (lambda () (decompose-flonum flo)) flonum->digits))  

(define (get-chr i)
  (integer->char (+ (char->integer #\0) i)))  
  
(define (flonum->string flo)
  (cond
    [(flzero? flo) "0.0"]
    [(flnan? flo) "+nan.0"]
    [(flinfinite? flo) 
      (if (flpositive? flo)
          "+inf.0"
          "-inf.0")]
    [else
      (let-values (((p r) (open-string-output-port)))
        (let* ((d (get-digits (flabs flo)))
               (k (car d))
               (n (cdr d)))
          (when (flnegative? flo)
            (put-string p "-"))
          (cond
            [(<= 0 k 9)
              ; print small postive exponent
              (let f ((i 0)(n n))
                (cond
                  [(null? n)
                    (if (< i k)
                        (begin
                          (put-string p "0")
                          (f (+ i 1) n))
                        (begin
                          (when (= i k)
                            (put-string p ".0"))
                          (r)))]
                  [else
                    (when (= i k)
                      (when (zero? k)
                        (put-string p "0"))
                      (put-string p "."))
                    (put-char p (get-chr (car n)))
                    (f (+ i 1) (cdr n))]))]
            [(<= -3 k 0)
              ; print small negative exponent
              (put-string p "0.")
              (let f ((i k))
                (unless (zero? i)
                  (put-string p "0")
                  (f (+ i 1))))
              (let f ((n n))
                (unless (null? n)
                  (put-char p (get-chr (car n)))
                  (f (cdr n))))
              (r)]
            [else
              ; print with exponent
              (put-char p (get-chr (car n)))
              (put-string p ".")
              (let f ((i 0)(n (cdr n)))
                (unless (null? n)
                  (put-char p (get-chr (car n)))
                  (f (+ i 1) (cdr n))))
              (put-string p "e")
              (display (- k 1) p)
              (r)])))]))
                
      
  

(flonum->string 1.0)
(flonum->string 2.0)
(flonum->string 3.0)

(flonum->string 1211.0)
(flonum->string 234534.0)
(flonum->string 35345234.0)

(flonum->string 1343.4567)
(flonum->string 2234534.456745)
(flonum->string 34564563456.7689678)

(flonum->string -1.23452345)
(flonum->string -2.2345234e-10)
(flonum->string -3.045345e-30)



(define-syntax trace
  (lambda (x)
    (syntax-case x (define)
      [(_ e ...)
        (letrec-syntax [(define (lambda (x) (syntax-case x (define) [(_ (e . args) b b* ...) #'(trace-define (e . args) b b* ...)])))]
          #'(begin e ...))])))


(trace-define-syntax eval-expr
  (lambda (x)
    (define (parse e)
      (syntax-case e ()
        [(l op r)
          (with-syntax ((r (parse #'(r))))
            #'(op l r))]
        [(l op r op* r* r** ...)
          (with-syntax ((r (parse #'(r))))
            (parse #'((op l r) op* r* r** ...)))]
        [((x x* ...))
          (parse #'(x x* ...))]          
        [(x) #'x]))
    (syntax-case x ()
      [(_ e e* ...)
        (parse #'(e e* ...))])))
         

(import (ironscheme clr))


(define-syntax with-clr-type2
  (lambda (x)
    (syntax-case x ()
      [(_ ((id type) ...) body body* ...)
        (for-all identifier? #'(id ... type ...))
          #' (let-syntax (
                  (id (lambda (x)
                        (define (lit=? id sym)
                          (eq? (syntax->datum id) sym))
                        (syntax-case x () 
                          [(_ : prop = value)
                            (and (identifier? #'prop) (lit=? #': ':) (lit=? #'= '=))
                            #'(clr-prop-set! type prop id value)]
                          [(_ : prop)
                            (and (identifier? #'prop) (lit=? #': ':))
                            #'(clr-prop-get type prop id)]
                          [(_ -> field = value)
                            (and (identifier? #'field) (lit=? #'-> '->) (lit=? #'= '=))
                            #'(clr-field-set! type field id value)]
                          [(_ -> field)
                            (and (identifier? #'field) (lit=? #'-> '->))
                            #'(clr-field-get type field id)]
                          [(_ meth . arg)
                            (or (identifier? #'meth) (string? (syntax->datum #'meth)))
                            #'(clr-call type meth id . arg)]
                          [(_ (arg arg* (... ...)) = value)
                            (lit=? #'= '=)
                            #'(clr-indexer-set! type id arg arg* (... ...) value)]
                          [(_ (arg arg* (... ...)))
                            #'(clr-indexer-get type id arg arg* (... ...))]
                          [(_ . args) 
                            (syntax-violation 'with-clr-type "invalid syntax" x #f)]
                          [_ #'id]))) ...)
                body body* ...);))
                ])))     
                
;(define-syntax declare-clr-type 
  ;(syntax-rules ()
    ;[(_ name                 

(import (ironscheme clr shorthand))
(clr-using IronScheme)

(let-clr-type ((obj (TestClass "foo"))) (obj : Message : Length))

(let-clr-type ((obj (TestClass "foo"))) (obj : Message : (1)))

(let-clr-type ((obj (TestClass "foo")))
  (obj : Message))


                          
(let ((obj (clr-new TestClass "foo"))) 
  (with-clr-type ((obj TestClass))
    (printf "~a\n" (eq? obj 50))
    (printf "~a\n" (obj : Message))
    (obj : Message = "world")
    (printf "~a\n" (obj -> Source))
    (obj -> Source = "bar")
    (printf "~a\n" (obj -> Source))
    (obj Print "hello")
    (printf "~a\n" (obj (1)))
    (obj(99) = #\a)
    (obj : Message)
    ))
    
(let-clr-type ((obj (TestClass "foo")))
  (obj : Message))

(define-syntax let-clr-type 
  (lambda (x)
    (syntax-case x ()
      [(_ ((id (type arg ...)) ...) b b* ...)
        (for-all identifier? #'(id ... type ...))
        #'(let ((id (clr-new type arg ...)) ...)
            (with-clr-type ((id type) ...)
              b b* ...))])))

    

(let-syntax ((mo (identifier-syntax :)))
  (let ((: 'doh))    
    (let ((obj (clr-new TestClass "foo"))) 
      (with-clr-type ((obj TestClass))
        (let ((: 'doof))
          (let-syntax ((mo (identifier-syntax :)))
            (obj mo Message)))))))
            
(define-syntax define-for-syntax
  (lambda (x)
    (syntax-case x ()
      [(_ (name . formals) b ...)
        #'(begin 
            (library (foo) 
              (export name)
              (import (rnrs))
              (define (name . formals) b ...))
            (import (foo) name))])))
            
            
(let ((k #'k))
  (with-syntax ((k k))
    (list k)))                
              
    
    
