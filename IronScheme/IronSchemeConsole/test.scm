
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

(define tak
  (lambda (x y z)
    (if (not (< y x))
        z
        (tak (tak (- x 1) y z)
             (tak (- y 1) z x)
             (tak (- z 1) x y) ))))

(time
 (tak 18 12 6) )

(newline)
;(display "Press any key to exit.")
;(read-char)
