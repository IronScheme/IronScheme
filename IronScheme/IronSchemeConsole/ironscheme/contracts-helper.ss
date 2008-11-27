(library (ironscheme contracts-helper)
  (export
    parse-body)
  (import 
    (ironscheme) 
    (ironscheme strings))
    
  (define (parse-body x)
    (define (get-name/type name)
      (let ((tokens (string-split 
                      (symbol->string (syntax->datum name)) 
                      ":")))
        (when (zero? (string-length (vector-ref tokens 0)))
          (syntax-violation 'get-name/type "length of argument > 0" name))
        (if (= 1 (vector-length tokens))
          (cons 
            (datum->syntax name
              (string->symbol (vector-ref tokens 0))) #f)
          (cons* 
            (datum->syntax name
              (string->symbol (vector-ref tokens 0))) 
            (datum->syntax name
              (string->symbol 
                (string-append (vector-ref tokens 1) "?")))
            (vector-ref tokens 1)))))
    (define (make-guard ai)
      (with-syntax ((n (car ai))
                    (g (cadr ai))
                    (s (string-append "not " (cddr ai))))
        #'(unless (g n)
            (assertion-violation #f s n))))
    (define (make-list-guard ai)
      (with-syntax ((n (car ai))
                    (g (cadr ai))
                    (s (string-append "not " (cddr ai))))
        #'(for-each (lambda (x) 
                     (unless (g x) (assertion-violation #f s x)))
                     n)))            
    (syntax-case x ()
      [((a ...) body body* ...)
        (for-all identifier? #'(a ...))
        (let ((ai (map get-name/type #'(a ...))))
          (with-syntax (((a ...) (map car ai))
                        ((g ...) (map make-guard (filter cdr ai)))) 
            #'((a ...)
                g ...
                (let ((a a) ...) body body* ...))))]
      [((a a* ... . rest) body body* ...)                
        (and (for-all identifier? #'(a a* ...)) (identifier? #'rest))
        (let ((ai (map get-name/type #'(a a* ...)))
              (ri (get-name/type #'rest)))
          (with-syntax (((a ...) (map car ai))
                        ((g ...) (map make-guard (filter cdr ai)))
                        (rest (car ri))
                        (h (if (cdr ri) (make-list-guard ri) #'#f))) 
            #'((a ... . rest)
                g ...
                h
                (let ((a a) ... (rest rest)) body body* ...))))]
      [(formals body body* ...)
        (identifier? #'formals)                
        (let ((ri (get-name/type #'formals)))
          (with-syntax ((formals (car ri))
                        (h (if (cdr ri) (make-list-guard ri) #'#f))) 
            #'(formals
                h
                (let ((formals formals)) body body* ...))))]
                ))
 
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
        
          


