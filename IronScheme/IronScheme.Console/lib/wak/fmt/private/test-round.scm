
(use fmt test)
;;(use numbers) ; test with and without numbers via -R numbers

(define (check-representation n)
  (define pence
    (inexact->exact (round (/ (modulo n 1000) 10))))
  (define pounds (quotient n 1000))

  (if (> pence 99)
      (begin
        (set! pence (- 100 pence))
        (set! pounds (add1 pounds))))

  (define expected-result
    (cond
     ((= pence 0) (sprintf "~S.00" pounds))
     ((< pence 10) (sprintf "~S.0~S" pounds pence))
     (else (sprintf "~S.~S" pounds pence))))

  (test (sprintf "~S = ~S?" (exact->inexact (/ n 1000)) expected-result)
      expected-result
    (fmt #f (num (/ n 1000) 10 2))))

(test-begin)
(for-each check-representation (iota 10000))
(test-end)
