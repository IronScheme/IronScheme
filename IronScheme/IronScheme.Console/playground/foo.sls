(library (foo)
  (export run)
  (import 
    (except (ironscheme) define + - < = time) 
    (ironscheme typed) 
    (ironscheme syntax) 
    (only (ironscheme datetime) measure)
    (rename (ironscheme unsafe)
        ($fx+   +)
        ($fx-   -)
        ($fx<?  <)
        ($fx=?  =)))

  (define-syntax-case (define (name arg ...) body ...)
    (with-syntax (((arg ...) (map (lambda (a) #`(#,a : fixnum))
                                #'(arg ...))))
      #'(define: (name arg ...) : fixnum
          body ...)))
                
  (define-syntax-rule (time expr)
    (measure 100 expr))

  (define (ack m n)
    (if (= m 0)
        (+ n 1)
        (if (= n 0)
            (ack (- m 1) 1)
            (ack (- m 1) (ack m (- n 1))))))
          
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
