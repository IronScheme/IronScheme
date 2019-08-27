(import 
  (rename (rnrs) (case-lambda rnrs:case-lambda))
  (only (ironscheme) printf)
  (srfi :102))

(define-syntax $
  (syntax-rules (=>)
    [(_ ex => er)
      (let ((ar ex))
        (let ((pass? (equal? ar er)))
          (or pass?
              (printf "Fail:     ~s\nActual:   ~s\nExpected: ~s\n\n" 'ex ar er))))]))
              
(define-syntax case-lambda
  (lambda (x)
    (define (gen-clause cls)
      (syntax-case cls (zero one two three four five
                        zero+ one+ two+ three+ four+ five+)
        [zero   #'(() #f)]
        [one    #'((a) #f)]
        [two    #'((a b) #f)]
        [three  #'((a b c) #f)]
        [four   #'((a b c d) #f)]
        [five   #'((a b c d e) #f)]
        [zero+  #'(a #f)]
        [one+   #'((a . b) #f)]
        [two+   #'((a b . c) #f)]
        [three+ #'((a b c . d) #f)]
        [four+  #'((a b c d . e) #f)]
        [five+  #'((a b c d e . f) #f)]))
    (syntax-case x ()
      [(_ cls ...)
        (with-syntax (((cls ...) (map gen-clause #'(cls ...))))
          #'(rnrs:case-lambda cls ...))])))

(let-syntax (($ (syntax-rules (=>)
                  [(_ ex => er)
                    ($ (procedure-arity ex) => er)])))
  
  ($ (case-lambda) => #f)
  
  ($ (case-lambda zero) => 0)
  ($ (case-lambda one) => 1)
  ($ (case-lambda two) => 2)
  ($ (case-lambda three) => 3)
  ($ (case-lambda four) => 4)
  ($ (case-lambda five) => 5)
 
  ($ (case-lambda zero+) => 0.0)
  ($ (case-lambda one+) => 1.0)
  ($ (case-lambda two+) => 2.0)
  ($ (case-lambda three+) => 3.0)
  ($ (case-lambda four+) => 4.0)
  ($ (case-lambda five+) => 5.0)
  
  ($ (case-lambda zero one) => '(0 1))
  ($ (case-lambda zero two) => '(0 2))
  ($ (case-lambda zero three) => '(0 3))
  ($ (case-lambda zero four) => '(0 4))
  ($ (case-lambda zero five) => '(0 5))

  ($ (case-lambda zero one+) => '(0 1.0))
  ($ (case-lambda zero two+) => '(0 2.0))
  ($ (case-lambda zero three+) => '(0 3.0))
  ($ (case-lambda zero four+) => '(0 4.0))
  ($ (case-lambda zero five+) => '(0 5.0)))  
  
(let-syntax (($ (syntax-rules (=>)
                  [(_ ex inc => er)
                    ($ (procedure-arity-includes? ex inc) => er)])))
  
  ($ (case-lambda) 0 => #f)
  ($ (case-lambda) 1 => #f)
  ($ (case-lambda) 2 => #f)
  ($ (case-lambda) 3 => #f)
  ($ (case-lambda) 4 => #f)
  ($ (case-lambda) 5 => #f)
  
  ($ (case-lambda zero) 0 => #t)
  ($ (case-lambda zero) 1 => #f)
  ($ (case-lambda zero) 2 => #f)
  ($ (case-lambda zero) 3 => #f)
  ($ (case-lambda zero) 4 => #f)
  ($ (case-lambda zero) 5 => #f)
  
  ($ (case-lambda one) 0 => #f)
  ($ (case-lambda one) 1 => #t)
  ($ (case-lambda one) 2 => #f)
  ($ (case-lambda one) 3 => #f)
  ($ (case-lambda one) 4 => #f)
  ($ (case-lambda one) 5 => #f)
  
  ($ (case-lambda two) 0 => #f)
  ($ (case-lambda two) 1 => #f)
  ($ (case-lambda two) 2 => #t)
  ($ (case-lambda two) 3 => #f)
  ($ (case-lambda two) 4 => #f)
  ($ (case-lambda two) 5 => #f)
  
  ($ (case-lambda three) 0 => #f)
  ($ (case-lambda three) 1 => #f)
  ($ (case-lambda three) 2 => #f)
  ($ (case-lambda three) 3 => #t)
  ($ (case-lambda three) 4 => #f)
  ($ (case-lambda three) 5 => #f)
  
  ($ (case-lambda four) 0 => #f)
  ($ (case-lambda four) 1 => #f)
  ($ (case-lambda four) 2 => #f)
  ($ (case-lambda four) 3 => #f)
  ($ (case-lambda four) 4 => #t)
  ($ (case-lambda four) 5 => #f)
  
  ($ (case-lambda five) 0 => #f)
  ($ (case-lambda five) 1 => #f)
  ($ (case-lambda five) 2 => #f)
  ($ (case-lambda five) 3 => #f)
  ($ (case-lambda five) 4 => #f)
  ($ (case-lambda five) 5 => #t)
 
  ($ (case-lambda zero+) 0 => #t)
  ($ (case-lambda zero+) 1 => #t)
  ($ (case-lambda zero+) 2 => #t)
  ($ (case-lambda zero+) 3 => #t)
  ($ (case-lambda zero+) 4 => #t)
  ($ (case-lambda zero+) 5 => #t)

  ($ (case-lambda one+) 0 => #f)
  ($ (case-lambda one+) 1 => #t)
  ($ (case-lambda one+) 2 => #t)
  ($ (case-lambda one+) 3 => #t)
  ($ (case-lambda one+) 4 => #t)
  ($ (case-lambda one+) 5 => #t)

  ($ (case-lambda two+) 0 => #f)
  ($ (case-lambda two+) 1 => #f)
  ($ (case-lambda two+) 2 => #t)
  ($ (case-lambda two+) 3 => #t)
  ($ (case-lambda two+) 4 => #t)
  ($ (case-lambda two+) 5 => #t)

  ($ (case-lambda three+) 0 => #f)
  ($ (case-lambda three+) 1 => #f)
  ($ (case-lambda three+) 2 => #f)
  ($ (case-lambda three+) 3 => #t)
  ($ (case-lambda three+) 4 => #t)
  ($ (case-lambda three+) 5 => #t)

  ($ (case-lambda four+) 0 => #f)
  ($ (case-lambda four+) 1 => #f)
  ($ (case-lambda four+) 2 => #f)
  ($ (case-lambda four+) 3 => #f)
  ($ (case-lambda four+) 4 => #t)
  ($ (case-lambda four+) 5 => #t)

  ($ (case-lambda five+) 0 => #f)
  ($ (case-lambda five+) 1 => #f)
  ($ (case-lambda five+) 2 => #f)
  ($ (case-lambda five+) 3 => #f)
  ($ (case-lambda five+) 4 => #f)
  ($ (case-lambda five+) 5 => #t)
  
  ($ (case-lambda zero one) 0 => #t)
  ($ (case-lambda zero one) 1 => #t)
  ($ (case-lambda zero one) 2 => #f)
  ($ (case-lambda zero one) 3 => #f)
  ($ (case-lambda zero one) 4 => #f)
  ($ (case-lambda zero one) 5 => #f)

  ($ (case-lambda zero two) 0 => #t)
  ($ (case-lambda zero two) 1 => #f)
  ($ (case-lambda zero two) 2 => #t)
  ($ (case-lambda zero two) 3 => #f)
  ($ (case-lambda zero two) 4 => #f)
  ($ (case-lambda zero two) 5 => #f)

  ($ (case-lambda zero three) 0 => #t)
  ($ (case-lambda zero three) 1 => #f)
  ($ (case-lambda zero three) 2 => #f)
  ($ (case-lambda zero three) 3 => #t)
  ($ (case-lambda zero three) 4 => #f)
  ($ (case-lambda zero three) 5 => #f)

  ($ (case-lambda zero four) 0 => #t)
  ($ (case-lambda zero four) 1 => #f)
  ($ (case-lambda zero four) 2 => #f)
  ($ (case-lambda zero four) 3 => #f)
  ($ (case-lambda zero four) 4 => #t)
  ($ (case-lambda zero four) 5 => #f)

  ($ (case-lambda zero five) 0 => #t)
  ($ (case-lambda zero five) 1 => #f)
  ($ (case-lambda zero five) 2 => #f)
  ($ (case-lambda zero five) 3 => #f)
  ($ (case-lambda zero five) 4 => #f)
  ($ (case-lambda zero five) 5 => #t)

  ($ (case-lambda zero one+) 0 => #t)
  ($ (case-lambda zero one+) 1 => #t)
  ($ (case-lambda zero one+) 2 => #t)
  ($ (case-lambda zero one+) 3 => #t)
  ($ (case-lambda zero one+) 4 => #t)
  ($ (case-lambda zero one+) 5 => #t)

  ($ (case-lambda zero two+) 0 => #t)
  ($ (case-lambda zero two+) 1 => #f)
  ($ (case-lambda zero two+) 2 => #t)
  ($ (case-lambda zero two+) 3 => #t)
  ($ (case-lambda zero two+) 4 => #t)
  ($ (case-lambda zero two+) 5 => #t)

  ($ (case-lambda zero three+) 0 => #t)
  ($ (case-lambda zero three+) 1 => #f)
  ($ (case-lambda zero three+) 2 => #f)
  ($ (case-lambda zero three+) 3 => #t)
  ($ (case-lambda zero three+) 4 => #t)
  ($ (case-lambda zero three+) 5 => #t)

  ($ (case-lambda zero four+) 0 => #t)
  ($ (case-lambda zero four+) 1 => #f)
  ($ (case-lambda zero four+) 2 => #f)
  ($ (case-lambda zero four+) 3 => #f)
  ($ (case-lambda zero four+) 4 => #t)
  ($ (case-lambda zero four+) 5 => #t)

  ($ (case-lambda zero five+) 0 => #t)
  ($ (case-lambda zero five+) 1 => #f)
  ($ (case-lambda zero five+) 2 => #f)
  ($ (case-lambda zero five+) 3 => #f)
  ($ (case-lambda zero five+) 4 => #f)
  ($ (case-lambda zero five+) 5 => #t)
  
  )    


