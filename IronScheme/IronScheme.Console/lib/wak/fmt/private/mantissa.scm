
;; Break a positive real number down to a normalized mantissa and
;; exponent. Default base=2, mant-size=52, exp-size=11 for IEEE doubles.

(define (mantissa+exponent num . opt)
  (if (zero? num)
      (list 0 0)
      (let-optionals* opt ((base 2) (mant-size 52) (exp-size 11))
        (let* ((bot (expt base mant-size))
               (top (* base bot)))
          (let lp ((n num) (e 0))
            (cond
              ((>= n top) (lp (quotient n base) (+ e 1)))
              ((< n bot) (lp (* n base) (- e 1)))
              (else (list n e))))))))
