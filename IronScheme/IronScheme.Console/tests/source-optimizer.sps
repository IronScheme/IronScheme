(import 
  (ironscheme)
  (source-optimizer optimize))
  
(define-syntax $
  (syntax-rules (=>)
    [(_ ex => er)
      (let ((ar (optimize 'ex)))
        (let ((pass? (equal? ar 'er)))
          (unless pass?
            (unless (eq? '_ 'er) 
              (printf "Fail:     ~s\nActual:   ~s\nExpected: ~s\n\n" 'ex ar 'er)))))]))


($ ((lambda (x) (begin (set! x 2) x)) 1) => 2)

($ (letrec ((f (lambda (x) (f x)))) f) => _)
($ (letrec ((f (lambda (x) (f x)))) +) => +)
($ (letrec ((f (lambda (x) (f x)))) -) => -)
($ (letrec ((f (lambda (x) (f x)))) *) => *)
($ (letrec ((f (lambda (x) (f x)))) /) => /)

($ ((lambda (x) (+ x)) 1) => 1)
($ ((lambda (x) (- x)) 1) => (- 1))
($ ((lambda (x) (* x)) 1) => (* 1))
($ ((lambda (x) (/ x)) 1) => (/ 1))

($ ((lambda (y) y) ((lambda (x) (+ x)) 1)) => 1)
($ ((lambda (y) y) ((lambda (x) (- x)) 1)) => (- 1))
($ ((lambda (y) y) ((lambda (x) (* x)) 1)) => (* 1))
($ ((lambda (y) y) ((lambda (x) (/ x)) 1)) => (/ 1))

($ (((lambda (x) (lambda (f) (f x))) 1) +) => 1)
($ (((lambda (x) (lambda (f) (f x))) 1) -) => (- 1))
($ (((lambda (x) (lambda (f) (f x))) 1) *) => (* 1))
($ (((lambda (x) (lambda (f) (f x))) 1) /) => (/ 1))

($ (lambda (x) (+ x)) => +)
($ (lambda (x) (- x)) => -)
($ (lambda (x) (* x)) => *)
($ (lambda (x) (/ x)) => /)

($ (begin 2 1) => 1)
($ (begin + 1) => 1)
($ (begin - 1) => 1)
($ (begin * 1) => 1)
($ (begin / 1) => 1)

($ (begin (+ 1) 1) => 1)
($ (begin (- 1) 1) => 1)
($ (begin (* 1) 1) => _)
($ (begin (/ 1) 1) => _)

($ (if #t 1 2) => 1)
($ (if #f 1 2) => 2)

($ (if 1 1 2) => 1)
($ (if + 1 2) => 1)
($ (if - 1 2) => 1)
($ (if * 1 2) => 1)
($ (if / 1 2) => _)

($ (if (+ 1) 1 2) => 1)
($ (if (- 1) 1 2) => 1)
($ (if (* 1) 1 2) => _)
($ (if (/ 1) 1 2) => _)

($ (if (not 1) 1 2) => 2)
($ (if (not #t) 1 2) => 2)
($ (if (not #f) 1 2) => 1)
($ (if (not +) 1 2) => 2)
($ (if (not /) 1 2) => _)

($ (if (lambda (x) x) 1 2) => 1)
($ (if (not (lambda (x) x)) 1 2) => 2)

($ (if (begin (+ 1) 1) 1 2) => 1)
($ (if (begin (- 1) 1) 1 2) => 1)
($ (if (begin (* 1) 1) 1 2) => _)
($ (if (begin (/ 1) 1) 1 2) => _)

($ (begin (lambda (x) x) 1) => 1)
($ (begin (not (lambda (x) x)) 1) => 1)




