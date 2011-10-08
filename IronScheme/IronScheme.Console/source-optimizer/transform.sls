(library (source-optimizer transform)
  (export
    transform-input
    transform-output)
  (import 
    (rnrs)
    (only (ironscheme) make-parameter parameterize)
    (source-optimizer match)
    (source-optimizer prelex)
    (source-optimizer primitive)
    (source-optimizer helpers))
    
  (define prelex-cache (make-parameter #f))
      
  (define prelex
    (lambda (x)
      (let ((ht (prelex-cache)))
        (let ((p (hashtable-ref ht x #f)))
          (or p
              (let ((p (make-prelex x #f)))
                (hashtable-set! ht x p)
                p))))))
    
  (define (in exp)
    (exp-case exp
      [(lambda (x) e)
        (list 'lambda (list (prelex x)) (in e))]
      [(letrec ((x e1)) e2)
        (list 'letrec (list (list (prelex x) (in e1))) (in e2))]
      [(begin e1 e2)
        (list 'begin (in e1) (in e2))]
      [(if e1 e2 e3)
        (list 'if (in e1) (in e2) (in e3))]
      [(set! x e)
        (list 'set! 
              (let ((p (prelex x))) 
                (set-prelex-source-assigned?! p #t)
                p) 
              (in e))]
      [(primitive p)
        (list 'primref p)]
      [else
        (match exp
          [(e ...)
            (cons 'call (map in e))]
          [e
            (cond
              [(symbol? e)
                (if (primitive? e) 
                    (list 'primref e)
                    (list 'ref 
                          (let ((p (prelex e))) 
                            (set-prelex-source-referenced?! p #t)
                            p)))]
              [else
                (list 'const e)])]
          [else
            (assertion-violation 'in "unknown expression" exp)])]))

  (define (out exp) 
    (exp-case exp
      [(const c) c]
      [(ref x) (prelex-name x)]
      [(begin e1 e2)
        (list 'begin (out e1) (out e2))]
      [(if e1 e2 e3)
        (list 'if (out e1) (out e2) (out e3))]
      [(set! x e)
        (list 'set! (prelex-name x) (out e))]
      [(call e1 e2)
        (list (out e1) (out e2))]
      [(primref p) p]
      [(lambda (x) e)
        (list 'lambda (list (prelex-name x)) (out e))]
      [(letrec ((x e1)) e2)
        (list 'letrec (list (list (prelex-name x) (out e1))) (out e2))]
      [else
        (assertion-violation 'out "invalid expression" exp)]))
    
  (define transform-input 
    (make-parameter
      (lambda (x)
        (parameterize [(prelex-cache (make-eq-hashtable))]
          (in x)))))
          
  (define transform-output (make-parameter out)))
    