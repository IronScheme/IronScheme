#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme syntax-utils)
  (export unexpand expand* syntax-trace)
  (import 
    (ironscheme))
    
  (define (self-evaluating? o)
    (or (boolean? o)
        (number? o)
        (string? o)
        (bytevector? o)
        (char? o))) 
        
  (define-syntax syntax-trace
    (lambda (x)
      (define (make-trace e p r)
        (with-syntax ((p p)(r r)(e e))
          #'(begin (printf "~a : ~a\n" 'p (syntax->datum e)) r)))
      (syntax-case x ()
        [(_ e (i ...) c ...)
          (with-syntax ((e* (car (generate-temporaries '(1)))))
            (with-syntax (((c ...) 
              (let f ((c #'(c ...))(a '()))
                (if (null? c)
                  (reverse a)
                  (syntax-case (car c) ()
                    [(p t r) (f (cdr c) (cons #`(p t #,(make-trace #'e* #'p #'r)) a))]
                    [(p r) (f (cdr c) (cons #`(p #t #,(make-trace #'e* #'p #'r)) a))])))))
            #'(let ((e* e)) (syntax-case e* (i ...) c ...))))])))        
        
  (define (pp-expr x)
    (define pe pp-expr)
    (define (pe* sl) 
      (map pe sl))
    (syntax-case x (case-lambda if primitive let begin or and lambda quote)
      [(if e1 e2 #f)
       (pe #`(and #,(pe #'e1) #,(pe #'e2)))]
      [(and e1 ... (and e2 ...)) 
       (pe #'(and e1 ... e2 ...))]
      [(and (and e1 ...) e2 ...) 
       (pe #'(and e1 ... e2 ...))]
      [(and expr ...)
       #`(and #,@(pe* #'(expr ...)))]
      [(let ((a b)) (if c d e))
       (and (free-identifier=? #'a #'c)
            (free-identifier=? #'d #'c))
       (pe #`(or #,(pe #'b) #,(pe #'e)))]
      [(or expr1 ... (or expr2 ...))
       (pe #'(or expr1 ... expr2 ...))]
      [(or (or expr1 ...) expr2 ...)
       (pe #'(or expr1 ... expr2 ...))]
      [(or expr ...)
       #`(or #,@(pe* #'(expr ...)))]      
      [(begin #f e) (pe #'e)]
      [((case-lambda [(a ...) b b* ...]) p ...)
        (with-syntax (((b ...) (pe* #'(b b* ...)))
                      ((a ...) (pe* #'(a ...)))
                      ((p ...) (pe* #'(p ...))))
          (pe #'(let ((a p) ...) b ...)))] 
      [(case-lambda [(a ...) b b* ...])
        (with-syntax (((b ...) (pe* #'(b b* ...)))
                      ((a ...) (pe* #'(a ...))))
          (pe #'(lambda (a ...) b ...)))] ; this needs to rerun again, somehow
      [(primitive proc) #'proc]
      [(quote c)
        (self-evaluating? (syntax->datum #'c))
        #'c]
      [(app a ...)
        (pe* #'(app a ...))]
      [i
        (identifier? #'i)
        (datum->syntax #'i (ungensym (syntax->datum #'i)))
        ]
      [_ x]))

  (define (unexpand x)
    (pretty-print (syntax->datum (pp-expr (datum->syntax #'here x))))
    (void))

  (define (expand* x)
    (let-values (((c r)(expand x (interaction-environment))))
      c))

  (define (test x)
    (unexpand (expand* x)))    
    
    
  ;(unexpand '(let ((a 1)) (if a a 2)))    

;(unexpand '(case-lambda
       ;((g$proc$4898$1WS4nL g$l$4899$1WS4nL)
        ;(if (null? g$l$4899$1WS4nL)
          ;'#f
          ;((case-lambda
             ;((g$e$4900$1WS4nL g$r$4901$1WS4nL g$proc$4902$1WS4nL)
              ;(if (g$proc$4902$1WS4nL g$e$4900$1WS4nL)
                ;g$e$4900$1WS4nL
                ;(g$find$4876$1WS4nL g$proc$4902$1WS4nL g$r$4901$1WS4nL))))
           ;(car g$l$4899$1WS4nL)
           ;(cdr g$l$4899$1WS4nL)
           ;g$proc$4898$1WS4nL)))))       
  
)    
  