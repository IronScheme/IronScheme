(import (rnrs))

(define-syntax test
  (syntax-rules ()
    [(_ e expected)
      (let ((r e))
        (display r)
        (display " => ")
        (display expected)
        (newline))]))

(test (let ((path '())
            (c #f))
        (let ((add (lambda (s)
                     (set! path (cons s path)))))
          (dynamic-wind
              (lambda () (add 'connect))
              (lambda ()
                (add (call-with-current-continuation
                      (lambda (c0)
                        (set! c c0)
                        'talk1))))
              (lambda () (add 'disconnect)))
          (if (< (length path) 4)
              (c 'talk2)
              (reverse path))))
      '(connect talk1 disconnect
                connect talk2 disconnect))

(test (let ((n 0))
        (call-with-current-continuation
         (lambda (k)
           (dynamic-wind
               (lambda ()
                 (set! n (+ n 1))
                 (k 1 2 3))
               (lambda ()
                 (set! n (+ n 2)))
               (lambda ()
                 (set! n (+ n 4))))))
        n) 
      1)
      
(test (let ((n 0))
          (call-with-current-continuation
           (lambda (k)
             (dynamic-wind
                 values
                 (lambda ()
                   (dynamic-wind
                       values
                       (lambda ()
                         (set! n (+ n 1))
                         (k #f))
                       (lambda ()
                         (set! n (+ n 2))
                         (k #f))))
                 (lambda ()
                   (set! n (+ n 4))))))
          n) 
        7)      

(test (let ((n 0)(c #f))
        (call-with-current-continuation
         (lambda (k)
           (dynamic-wind
               values
               (lambda ()
                 (dynamic-wind
                     values
                     (lambda ()
                       (set! n (+ n 1))
                       (call/cc (lambda (c0) (set! c c0)))
                       (k 'thunk))
                     (lambda ()
                       (set! n (+ n 2))
                       (k 'after))))
               (lambda ()
                 (set! n (+ n 4))))))
        (if (= n 7)
          (c 'next)                 
          n)) 
      13) 
      
(test (let ((n 0)(c #f))
        (call-with-current-continuation
         (lambda (k)
           (dynamic-wind
               values
               (lambda ()
                 (dynamic-wind
                     values
                     (lambda ()
                       (set! n (+ n 1))
                       (k 'thunk))
                     (lambda ()
                       (set! n (+ n 2))
                       (call/cc (lambda (c0) (set! c c0)))
                       (k 'after))))
               (lambda ()
                 (set! n (+ n 4))))))
        (if (= n 7)
          (c 'next)                 
          n)) 
      11)  
      
(test (let ((n 0)(c #f))
        (call-with-current-continuation
         (lambda (k)
           (dynamic-wind
               values
               (lambda ()
                 (dynamic-wind
                     (lambda ()
                        (call/cc (lambda (c0) (set! c c0))))
                     (lambda ()
                       (set! n (+ n 1))
                       (k 'thunk))
                     (lambda ()
                       (set! n (+ n 2))
                       (k 'after))))
               (lambda ()
                 (set! n (+ n 4))))))
        (if (= n 7)
          (c 'next)                 
          n)) 
      14)    
      
(test (let ((n 0)(c #f))
        (call-with-current-continuation
         (lambda (k)
           (dynamic-wind
               values
               (lambda ()
                 (dynamic-wind
                     (lambda ()
                       (set! n (+ n 8)))
                     (lambda ()
                       (set! n (+ n 1))
                       (call/cc (lambda (c0) (set! c c0)))
                       (k 'thunk))
                     (lambda ()
                       (set! n (+ n 2))
                       (k 'after))))
               (lambda ()
                 (set! n (+ n 4))))))
        (if (= n 15)
          (c 'next)                 
          n)) 
      29)   
      
(test (let ((n 0)(c #f))
        (call-with-current-continuation
         (lambda (k)
           (dynamic-wind
               values
               (lambda ()
                 (dynamic-wind
                     (lambda ()
                        (call/cc (lambda (c0) (set! c c0)))
                        (k 'before))
                     (lambda ()
                       (set! n (+ n 1))
                       (k 'thunk))
                     (lambda ()
                       (set! n (+ n 2))
                       (k 'after))))
               (lambda ()
                 (set! n (+ n 4))))))
        (if (= n 4)
          (c 'next)                 
          n)) 
      8) 
      
      
(test 
  (dynamic-wind
    values
    (lambda ()
      (let ((n 0)(c #f))
        (call/cc 
          (lambda (k)
            (dynamic-wind
              values
              (lambda ()
                (dynamic-wind
                   (lambda ()
                     (set! n (+ n 1)))
                   (lambda ()
                     (call/cc (lambda (c0) (set! c c0)))
                     (set! n (+ n 2))
                     (k))
                   (lambda ()
                     (set! n (+ n 4)))))
               values)))
          (if (= n 7)
            (dynamic-wind
              values                
              (lambda ()
                (dynamic-wind
                  (lambda ()
                    (set! n (- n 1)))
                  (lambda ()
                    (c))
                  (lambda ()
                    (set! n (- n 4)))))
              values)                  
            n)))
      values)
      9)                        
      
(display 'done)
(newline)

            