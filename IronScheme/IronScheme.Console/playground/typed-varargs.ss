(import (ironscheme typed))

(define (test-lambda)
  (let ([f (lambda: 
            ((x : fixnum) (y : fixnum) #(rest) -> fixnum)
             (writeln (length rest))
             (+ x y))])
    (displayln (procedure-form f))
    (f 10 20 2 1)
    (f 10 20)
    (f 10 20 2)
    f))
((test-lambda) 10 20 2 3 4 5)

(define test (lambda: ((x1 : flonum) #(rest) -> flonum) (length rest)))


(define (test-lambda)
  (let ([f (case-lambda: 
            ;[() #f]
            [((x : fixnum) (y : fixnum) #(rest) -> fixnum)
             (writeln (length rest))
             (+ x y)])])
    (displayln (procedure-form f))             
    (f 10 20 2 3 4 5)))
(test-lambda)


(define (test-lambda)  
  (let ([f (lambda: ((x : fixnum) (y : fixnum) #(rest) -> fixnum)
             (display (list x y rest))
                          (newline)\n             (+ x y))])\n   
                          
                           (display \"Testing lambda: with 2 args (empty rest): \")\n    (f 10 20)\n    
                           (display \"Testing lambda: with 3 args: \")\n    (f 10 20 30)))\n\n(define (test-case-lambda)\n  (let ([f (case-lambda:\n             [((x : fixnum) (y : fixnum) #(rest) -> fixnum)\n            
                             (display (list x y rest))\n              (newline)\n              (+ x y)])])\n   
                              (display \"Testing case-lambda: with 2 args (empty rest): \")\n    (f 10 20)\n    
                              (display \"Testing case-lambda: with 3 args: \")\n    (f 10 20 30)\n    
                              (display \"Testing case-lambda: with 4 args: \")\n    (f 10 20 30 40)))\n\n(test-lambda)\n(newline)\n(test-case-lambda)"
