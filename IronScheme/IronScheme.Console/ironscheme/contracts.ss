(library (ironscheme contracts)
  (export
    case/contract
    lambda/contract
    define/contract
    let/contract)
  (import 
    (ironscheme) 
    (ironscheme contracts-helper))
    
  (define-syntax lambda/contract  
    (lambda (x)
      (syntax-case x ()
        [(_ e1 e2 e* ...)
          (with-syntax (((e ...) (parse-body #'(e1 e2 e* ...))))
            #'(lambda e ...))])))

  (define-syntax case/contract  
    (lambda (x)
      (syntax-case x ()
        [(_ e ...)
          (with-syntax (((e ...) (map parse-body #'(e ...))))
            #'(case-lambda e ...))])))
                  
  (define-syntax define/contract
    (lambda (x)
      (syntax-case x ()
        [(_ (name . formals) body body* ...)
          (identifier? #'name)
          #'(define name (lambda/contract formals body body* ...))])))
          
  (define-syntax let/contract
    (lambda (x)
      (syntax-case x ()
        [(_ ((n v) ...) body body* ...)
          #'((lambda/contract (n ...) body body* ...) v ...)]
        [(_ f ((n v) ...) body body* ...)
          (identifier? #'f)
          #'(letrec ((f (lambda/contract (n ...) body body* ...))) (f v ...))]        
          )))        
      
       
)   

#|
      
(define/type (a f:boolean . a:integer) 
  (define/type (b x:boolean) x)
  (b f))

(define foo (let/type f ((a:integer 10)(b:list '()))
              (if (zero? a)
                b
                (f (- a 1)(cons a b)))))
                
(printf "~a\n" foo)                
        
(printf "~a\n" ((lambda/type (a:integer-valued b:boolean) (cons a b)) 1 '#f))

(printf "~a\n" ((lambda/type a:integer a) 1 2 3))

(printf "~a\n" (a #f 1 3 4))
|#      
        
          


