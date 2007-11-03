
(load-diags)

;; test mother
(let ((b '(1 2 3)))
  (let* ((a b)
         (b (cons 0 a)))
    (let bar ((a b))
      (if (null? a)
          (begin
            (display "eureka!")
            (newline))
          (begin
            (display a)
            (newline)
            (bar (cdr a)))))))

;; note how this will be slower, as it  refers to the global variable
(define (tak x y z)
  (if (not (< y x))
    z
    (tak (tak (- x 1) y z)
         (tak (- y 1) z x)
         (tak (- z 1) x y) )))

;; this version is optimized a little, but makes a big difference
(define taki
  (letrec ((taki 
    (lambda (x y z)
      (if (not (< y x))
        z
        (taki (taki (- x 1) y z)
             (taki (- y 1) z x)
             (taki (- z 1) x y) )))))
    taki))

(display "tak:  ")
(time
 (tak 18 12 6) )

(display "taki: ")
(time
 (taki 18 12 6) )

(display "tak:  ")
(time
 (tak 22 14 8) )

(display "taki: ")
(time
 (taki 22 14 8) )


(define-syntax for
  (syntax-rules (in into)
    ((_ a in b f ...)
     (for-each (lambda (a) (begin f ...)) b))
    ((_ a t b f)
     (for-each (lambda (k) ((lambda (a) f) (t k)))  b))
    ((_ a into b f ... )
     (for-each (lambda (a) f ...) b))))
     
(for c in (string->list "hello")
  (begin
    (display (char-upcase c))))

(newline)

(for c into (string->list "hello")
    (display c)
    (newline))

(for c char-upcase (string->list "hello") (display c))

(newline)




;(display "Press any key to exit.")
;(read-char)
