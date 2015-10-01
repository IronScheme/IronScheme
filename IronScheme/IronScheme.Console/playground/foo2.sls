(library (foo2)
  (export run)
  (import 
    (except (ironscheme) define + - < = time fx+ fx- fx<? fx=?) 
    (ironscheme typed core) 
    (ironscheme syntax shorthand) 
    (only (ironscheme datetime) measure)
    (rename (only (ironscheme typed fixnums) fx+ fx- fx<? fx=?)
        (fx+   +)
        (fx-   -)
        (fx<?  <)
        (fx=?  =)))

  (define-syntax-rule (define (name arg ...) body ...)
    (define: (name (arg : fixnum) ... -> fixnum)
        body ...))
      
  (define-syntax-rule (time expr)
    (measure 1000 expr))

  (define (ack m n)
    (cond
      [(= m 0) (+ n 1)]
      [(= n 0) (ack (- m 1) 1)]
      [else (ack (- m 1) (ack m (- n 1)))]))
          
  (define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))

  (define (tak x y z)
    (if (not (< y x))
        z
        (tak (tak (- x 1) y z)
             (tak (- y 1) z x)
             (tak (- z 1) x y))))      
    
  (define (run)
    (ack 3 1)
    (time (ack 3 9))
    (fib 10)
    (time (fib 33))
    (tak 18 12 6)
    (time (tak 21 16 6))
    0))
