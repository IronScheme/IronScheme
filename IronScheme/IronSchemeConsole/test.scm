
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

(newline)

(display "taki: ")
(time
 (taki 18 12 6) )

(newline)

(display "tak:  ")
(time
 (tak 18 12 6) )

(newline)

(display "taki: ")
(time
 (taki 18 12 6) )

(newline)


(define-syntax for
  (syntax-rules (in into)
    ((_ a in b f ...)
     (for-each (lambda (a) (begin f ...)) b))
    ((_ a b f)
     (for-each (lambda (a) f) b))
    ((_ a into b f ...)
     (for-each (lambda (a) f ...) b))))
     
(for c in (string->list "hello")
  (begin
    (display (char-upcase c))))

(newline)



;(display "Press any key to exit.")
;(read-char)
