
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









