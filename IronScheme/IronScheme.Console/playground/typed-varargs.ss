(import (ironscheme typed))

(define (test-lambda)
  (let ([f (lambda: 
            ((x : fixnum) (y : fixnum) rest ... -> fixnum)
             (writeln (length rest))
             (+ x y))])
    (displayln (procedure-form f))
    (f 10 20 2 1)
    (f 10 20)
    (f 10 20 2)
    f))
((test-lambda) 10 20 2 3 4 5)

(define test (lambda: ((x1 : flonum) rest ... -> flonum) (length rest)))


(define (test-lambda)
  (let ([f (case-lambda: 
            ;[() #f]
            [((x : fixnum) (y : fixnum) rest ... -> fixnum)
             (writeln (length rest))
             (+ x y)])])
    (displayln (procedure-form f))             
    (f 10 20 2 3 4 5)))
(test-lambda)

;; Greeting function with multiple arities
(define greeting
  (case-lambda:
    [( -> string)                              ; 0 args - empty param list
      "Hello, World!"]
    [((name : string) -> string)               ; 1 arg
      (string-append "Hello, " name "!")]
    [((name : string) others ... -> string)    ; 1+ args with varargs
      (string-append "Hello, " name " and " 
                     (number->string (length others)) 
                     " others!")]))
;; Usage:
(greeting)                        ; "Hello, World!"
(greeting "Alice")                ; "Hello, Alice!"
(greeting "Alice" "Bob")          ; "Hello, Alice and 1 others!"
(greeting "Alice" "Bob" "Charlie" "David")  ; "Hello, Alice and 3 others!"

; bad syntax, looks natural, but is not
;(lambda: (() -> flonum) 3.14159)


(import (ironscheme) (ironscheme typed))
(define (a) 
  (letrec*: 
    (((a : fixnum) 10)
     ((b : fixnum) (+ a 5))
     ((c : fixnum) (+ b 3))
     -> fixnum)
    (+ a b c)))
(disassemble a)