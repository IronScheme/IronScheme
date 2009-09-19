(library (source-optimizer cpletrec)
  (export 
    cpletrec)
  (import 
    (ironscheme)
    (source-optimizer prelex)
    (as-match))
    
  (define (make-bind lhs* rhs* body) #f)   
  
  (define (unique-prelex x) 
    (let ([x (make-prelex (prelex-name x) (prelex-operand x))])
      (set-prelex-source-referenced?! x #t)
      x))

  (define (build-assign* lhs* rhs* body)
    (define (mark-assigned! lhs)
      ;;; FIXME: this is very fragile
      (unless (prelex-source-assigned? lhs) 
        (set-prelex-source-assigned?! lhs 
           (or (prelex-global-location lhs) #t))))
    (for-each mark-assigned! lhs*)
    (let f ([lhs* lhs*] [rhs* rhs*])
      (cond
        [(null? lhs*) body]
        [else 
         (make-seq
           (make-assign (car lhs*) (car rhs*))
           (f (cdr lhs*) (cdr rhs*)))])))  

  (define simple-primitives '())

  (define (extend-hash lhs* h ref)
    (for-each (lambda (lhs) (hashtable-set! h lhs #t)) lhs*)
    (lambda (x)
      (unless (hashtable-ref h x #f)
        (hashtable-set! h x #t)
        (ref x))))
        
  (define (E* x* ref comp)
    (cond
      [(null? x*) '()]
      [else
       (cons (E (car x*) ref comp)
             (E* (cdr x*) ref comp))]))  
             
  (define (do-rhs* i lhs* rhs* ref comp vref vcomp)
    (cond
      [(null? rhs*) '()]
      [else
       (let ([h (make-eq-hashtable)]
             [rest (do-rhs* (fxadd1 i) lhs* (cdr rhs*) ref comp vref vcomp)])
         (let ([ref
                (lambda (x)
                  (unless (hashtable-ref h x #f)
                    (hashtable-set! h x #t)
                    (ref x)
                    (when (memq x lhs*)
                      (vector-set! vref i #t))))]
               [comp
                (lambda ()
                  (vector-set! vcomp i #t)
                  (comp))])
           (cons (E (car rhs*) ref comp) rest)))]))
           
  (define (partition-rhs* i lhs* rhs* vref vcomp)
    (cond
      [(null? lhs*) (values '() '() '() '() '() '())]
      [else
       (let-values 
         ([(slhs* srhs* llhs* lrhs* clhs* crhs*)
           (partition-rhs* (fxadd1 i) (cdr lhs*) (cdr rhs*) vref vcomp)]
          [(lhs rhs) (values (car lhs*) (car rhs*))])
         (cond
           [(prelex-source-assigned? lhs)
            (values slhs* srhs* llhs* lrhs* (cons lhs clhs*) (cons rhs crhs*))]
           [(clambda? rhs)
            (values slhs* srhs* (cons lhs llhs*) (cons rhs lrhs*) clhs* crhs*)]
           [(or (vector-ref vref i) (vector-ref vcomp i))
            (values slhs* srhs* llhs* lrhs* (cons lhs clhs*) (cons rhs crhs*))]
           [else
            (values (cons lhs slhs*) (cons rhs srhs*) llhs* lrhs* clhs* crhs*)]
           ))]))
           
  (define (do-recbind lhs* rhs* body ref comp letrec?) 
    (let ([h (make-eq-hashtable)]
          [vref (make-vector (length lhs*) #f)]
          [vcomp (make-vector (length lhs*) #f)])
      (let* ([ref (extend-hash lhs* h ref)]
             [body (E body ref comp)])
        (let ([rhs* (do-rhs* 0 lhs* rhs* ref comp vref vcomp)])
          (let-values ([(slhs* srhs* llhs* lrhs* clhs* crhs*)
                        (partition-rhs* 0 lhs* rhs* vref vcomp)])
            ;;; (let ([made-complex 
            ;;;        (filter (lambda (x) (not (var-assigned x)))
            ;;;                clhs*)])
            ;;;   (unless (null? made-complex)
            ;;;     (set! complex-count 
            ;;;       (+ complex-count (length made-complex)))
            ;;;     (printf "COMPLEX (~s) = ~s\n" 
            ;;;             complex-count 
            ;;;             (map unparse made-complex))))
            (let ([void* (map (lambda (x) `(const ,(void))) clhs*)])
              (make-bind slhs* srhs*
                (make-bind clhs* void*
                  (make-fix llhs* lrhs*
                    (if letrec?
                        (let ([t* (map unique-prelex clhs*)])
                          (make-bind t* crhs*
                            (build-assign* clhs* t* body)))
                        (build-assign* clhs* crhs* body)))))))))))
                        
  (define (E x ref comp)
    (struct-case x
      [(constant) x]
      [(prelex) (ref x) x]
      [(assign lhs rhs)
       (ref lhs)
       (comp)
       (make-assign lhs (E rhs ref comp))]
      [(primref) x]
      [(bind lhs* rhs* body)
       (let ([rhs* (E* rhs* ref comp)])
         (let ([h (make-eq-hashtable)])
           (let ([body (E body (extend-hash lhs* h ref) comp)])
             (make-bind lhs* rhs* body))))]
      [(recbind lhs* rhs* body)
       (if (null? lhs*)
           (E body ref comp)
           (do-recbind lhs* rhs* body ref comp #t))]
      [(rec*bind lhs* rhs* body)
       (if (null? lhs*)
           (E body ref comp)
           (do-recbind lhs* rhs* body ref comp #f))] 
      [(conditional e0 e1 e2)
       (make-conditional (E e0 ref comp) (E e1 ref comp) (E e2 ref comp))]
      [(seq e0 e1) (make-seq (E e0 ref comp) (E e1 ref comp))]
      [(clambda g cls* cp free name)
       (make-clambda g
         (map (lambda (x)
                (struct-case x
                  [(clambda-case info body)
                   (let ([h (make-eq-hashtable)])
                     (let ([body (E body (extend-hash (case-info-args info) h ref) void)])
                       (make-clambda-case info body)))]))
              cls*)
         cp free name)]
      [(funcall rator rand*)
       (let ([rator (E rator ref comp)] [rand* (E* rand* ref comp)])
         (struct-case rator
           [(primref op)
            (unless (memq op simple-primitives)
              (comp))]
           [else
            (comp)])
         (make-funcall rator rand*))]
      [(mvcall p c)
       (let ([p (E p ref comp)] [c (E c ref comp)])
         (comp)
         (make-mvcall p c))]
      [(forcall rator rand*) 
       (make-forcall rator (E* rand* ref comp))]
      [else (error who "invalid expression" (unparse x))]))
        
  (define (cpletrec x)        
    (E x (lambda (x) (error who "free var found" x))
         void)) )  