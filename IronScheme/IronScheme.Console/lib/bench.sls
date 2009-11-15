(library (bench)
  (export bench)
  (import 
    (ironscheme)
    (ironscheme clr))

  (define (make-stopwatch)
    (clr-static-call System.Diagnostics.Stopwatch StartNew))
    
  (define (elapsed-milliseconds sw)
    (clr-prop-get #f TotalMilliseconds 
      (clr-prop-get System.Diagnostics.Stopwatch Elapsed sw)))
      
  (define (print expr iters ms)
    (let* ((base (* iters baseline))
           (ms (fl- ms base)))
      (display "Benchmark:  ")
      (write expr)
      (newline)
      (display "Iterations: ")
      (write iters)
      (newline)
      (display "Total ms:   ")
      (display (clr-static-call String Format "{0:F3}" ms))
      (newline)
      (display "Average ms: ")
      (display (clr-static-call String Format "{0:F6}" (/ ms iters)))
      (newline)
      (newline)))
      
  (define-syntax bench
    (syntax-rules ()
      [(_ expr)
        (bench expr 10000.0)]
      [(_ expr max-ms)
        (bench expr max-ms print)]
      [(_ expr max-ms cont)
        (let ((sw (make-stopwatch)))
          (let loop ((i 0))
            (let ((ms (elapsed-milliseconds sw)))
              (cond
                [(fl>=? ms max-ms)
                  (cont 'expr i ms)]
                [else
                  expr
                  (loop (fx+ i 1))]))))]))
          
  (define baseline (bench '() 1000.0 (lambda (e i ms) (/ ms i)))))
