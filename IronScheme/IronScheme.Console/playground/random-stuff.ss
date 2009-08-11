
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
         

