;;; Oscar Waddell. "Extending the Scope of Syntactic Abstraction". PhD.
;;; Thesis. Indiana University Computer Science Department. August 1999.
;;; Available online: 
;;;   http://www.cs.indiana.edu/~owaddell/papers/thesis.ps.gz

(library (source-optimizer cp0)
  (export
    cp0-optimize
    cp0-effort-limit 
    cp0-size-limit)
  (import 
    (rnrs)
    (only (ironscheme) make-parameter void)
    (source-optimizer prelex)
    (source-optimizer primitive)
    (source-optimizer transform)
    (source-optimizer helpers))
    
  (define cp0-effort-limit (make-parameter 50))
  (define cp0-size-limit (make-parameter 8))  
  
  (define-structure (app opnd ctxt)
    ([inlined #f]))

  (define-structure (operand expr env ec)
    ([value                  #f]
     [residualize-for-effect #f]
     [size                    0]
     [single-reference-size   0]
     [inner-pending          #f]
     [outer-pending          #f]))
     
  (define-structure (counter value ctxt k))

  (define (passive-counter)
    (make-counter (greatest-fixnum) #f
      (lambda args 
        (error 'passive-counter "invalid abort"))))

  (define (passive-counter-value x)
    (- (greatest-fixnum) (counter-value x)))

  (define (active-counter? x)
    (and (counter? x) (counter-ctxt x)))

  (define (decrement x amt)
    (let ([n (- (counter-value x) amt)])
      (set-counter-value! x n)
      (when (< n 0)
        (reset-integrated! (counter-ctxt x))
        ((counter-k x) #f))))

  (define (abort-counter! x)
    (reset-integrated! (counter-ctxt x))
    ((counter-k x) #f))

  (define (reset-integrated! ctxt)
    (set-app-inlined! ctxt #f)
    (let ([ctxt (app-ctxt ctxt)])
      (when (app? ctxt)
        (reset-integrated! ctxt))))       
  
  (define-syntax context-case
    (lambda (stx)
      (define (test x)
        (case (syntax->datum x)
          [(test)   #'(eq? t 'test)]
          [(value)  #'(eq? t 'value)]
          [(effect) #'(eq? t 'effect)]
          [(app)    #'(app? t)]
          [else (syntax-violation stx "invalid context" x)]))
      (define (extract cls*)
        (syntax-case cls* (else)
          [() #'(error 'extract "unmatched context" t)]
          [([else e e* ...]) #'(begin e e* ...)]
          [([(t* ...) e e* ...] rest ...) 
           (with-syntax ([(t* ...) (map test #'(t* ...))]
                         [body (extract #'(rest ...))])
             #'(if (or t* ...) 
                   (begin e e* ...)
                   body))]))
      (syntax-case stx ()
        [(_ expr cls* ...)
         (with-syntax ([body (extract #'(cls* ...))])
           #'(let ([t expr])
               body))]))) 
           
  (define (cp0 exp ctxt env ec sc)
    (decrement ec 1)
    (exp-case exp
      [(const c) 
        exp]
      [(ref x) 
        (cp0-ref x ctxt env ec sc)]
      [(begin e1 e2)
        (let ((e1 (cp0 e1 'effect env ec sc))
              (e2 (cp0 e2 ctxt env ec sc)))
          (make-seq e1 e2))]
      [(if e1 e2 e3)
        (let ((e1 (cp0 e1 'test env ec sc)))
          (exp-case (result-exp e1)
            [(const c) 
              (make-seq e1 (cp0 (if c e2 e3) ctxt env ec sc))]
              [else
                (let ((ctxt (if (app? ctxt) 'value ctxt)))
                  (let ((e2 (cp0 e2 ctxt env ec sc))
                        (e3 (cp0 e3 ctxt env ec sc)))
                    (if (record-equal? e2 e3 ctxt)
                        (make-seq e1 e2)
                        (begin
                          (decrement sc 1)
                          `(if ,e1 ,e2 ,e3)))))]))]
      [(set! x e)
        (let ((x (lookup x env)))
          (make-seq
            (if (not (prelex-source-referenced? x))
                (cp0 e 'effect env ec sc)
                (let ((e (cp0 e 'value env ec sc)))
                  (decrement sc 1)
                  (set-prelex-residual-assigned?! x #t)
                  `(set! ,x ,e)))
            `(const ,(void))))]
      [(call e1 e2)
        (cp0-call e1 (make-operand e2 env ec) ctxt env ec sc)]
      [(primref p)
        (context-case ctxt
          [(app)
            (fold-prim p ctxt sc)]
          [(value)
            (decrement sc 1)
            exp]
          [(test effect)
            `(const #t)])]
      [(lambda (x) e)
        (context-case ctxt
          [(app) 
            (inline exp ctxt env ec sc)]
          [(value)
            (decrement sc 1)
            (with-extended-env ((env x) (env x #f))
              `(lambda (,x) ,(cp0 e 'value env ec sc)))]
          [(test effect)
            `(const #t)])]
      [(letrec ((x e1)) e2)
        (with-extended-env ((env x) (env x #f))
          (let ((opnd (make-operand e1 env ec)))
            (set-prelex-operand! x opnd)
            (let ((e2 (cp0 e2 ctxt env ec sc)))
              (if (or (not (prelex-residual-referenced? x))
                      (exp-case e2
                        [(const c) #t]
                        [(primref p) #t]
                        [else #f]))
                  e2
                  (begin 
                    (decrement sc (+ 1 (operand-size opnd)))
                    `(letrec ((,x ,(operand-value opnd))) ,e2))))))]))

  (define (make-seq e1 e2)
    ;;; returns a (seq e0 e1) with a seq-less e1 if both 
    ;;; e0 and e1 are constructed properly.
    (if (simple? e1)
        e2
        (let ([e1 (exp-case e1
                    [(begin e1a e1b) (if (simple? e1b) e1a e1)]
                    [else e1])])
          (exp-case e2
            [(begin e3 e4) 
              `(begin (begin ,e1 ,e3) ,e4)]
            [else 
              `(begin ,e1 ,e2)]))))

  ;;; simple?: check quickly whether something is effect-free
  (define (simple? x) 
    (exp-case x
      [(const c) #t]
      [(ref x) #t]
      [(lambda (x) body) #t]
      [(primref p) #t]
      [else #f]))

  ;;; result returns the "last" value of an expression
  (define (result-exp e)
    (exp-case e
      [(begin e1 e2) e2]
      [else e]))

  (define (record-equal? e1 e2 ctxt)
    (exp-case e1
      [(const c1)
       (exp-case e2
         [(const c2)
          (context-case ctxt
            [(effect) #t]
            [(test) (if c1 c2 (not c2))]
            [else (eq? c1 c2)])]
         [else #f])]
      [else #f]))

  (define (residualize-operand opnd e2 sc)
    (if (not (operand-residualize-for-effect opnd))
        e2
        (or (and (operand-single-reference-size opnd)
                 (or (not (operand-value opnd))
                     (simple? (operand-value opnd)))
                 (begin
                   (decrement sc (operand-single-reference-size opnd))
                   e2))
            (let ((e1 (or (operand-value opnd)
                          (struct-case opnd
                            [(operand expr env ec)
                              (cp0 expr 'effect env ec sc)]))))
              (if (simple? e1)
                  e2
                  (begin
                    (decrement sc (operand-size opnd))
                    (make-seq e1 e2)))))))
                 
  (define (value-visit-operand! opnd)
    (or (operand-value opnd)
        (let ([sc (passive-counter)])
          (let ([e (struct-case opnd
                     [(operand expr env ec) 
                      (cp0 expr 'value env sc ec)])])
            (set-operand-value! opnd e)
            (set-operand-size! opnd (passive-counter-value sc))
            e))))
            
  (define (score-value-visit-operand! opnd sc)
    (let ([val (value-visit-operand! opnd)])
      (let ([score (operand-size opnd)])
        (decrement sc score))
      val))
      
  (define (cp0-call e opnd ctxt env ec sc)
    (let ([ctxt (make-app opnd ctxt)])
      (let ([e (cp0 e ctxt env ec sc)])
        (if (app-inlined ctxt)
            (residualize-operand opnd e sc)
            (begin
              (decrement sc 1)
              `(call ,e ,(score-value-visit-operand! opnd sc)))))))

  (define (cp0-ref x ctxt env ec sc)
    (context-case ctxt
      [(effect) `(const ,(void))]
      [else 
       (let ([new-x (lookup x env)])
         (when (eq? new-x x)
           (set-prelex-source-singly-referenced?! new-x #f))
         (let ([opnd (prelex-operand new-x)])
           (if (and opnd (not (operand-inner-pending opnd)))
               (or (cp0-single-ref new-x opnd ctxt env ec sc)
                   (begin
                     (dynamic-wind
                       (lambda () (set-operand-inner-pending! opnd #t))
                       (lambda () (value-visit-operand! opnd))
                       (lambda () (set-operand-inner-pending! opnd #f)))
                     (if (prelex-source-assigned? new-x)
                         (residualize-ref new-x sc)
                         (copy new-x opnd ctxt ec sc))))
               (residualize-ref new-x sc))))]))

  (define (copy x opnd ctxt ec sc)
    (let ([rhs (result-exp (operand-value opnd))])
      (exp-case rhs
        [(const c) rhs]
        [(ref y)
         (if (prelex-source-assigned? y)
             (residualize-ref x sc)
             (let ([opnd (prelex-operand y)])
               (unless (prelex-source-singly-referenced? x)
                 (set-prelex-source-singly-referenced?! y #f))
               (if (and opnd (operand-value opnd))
                   (copy2 y opnd ctxt ec sc)
                   (residualize-ref y sc))))]
        [else (copy2 x opnd ctxt ec sc)])))

  (define (copy2 x opnd ctxt ec sc)
    (let ([rhs (result-exp (operand-value opnd))])
      (exp-case rhs
        [(lambda (z) body) 
         (context-case ctxt
           [(value) (residualize-ref x sc)]
           [(test) `(const #t)]
           [(app) 
            (or (and (not (operand-outer-pending opnd))
                     (dynamic-wind
                       (lambda () (set-operand-outer-pending! opnd #t))
                       (lambda ()
                         (call/cc
                           (lambda (abort)
                             (inline rhs ctxt '() ;empty-env 
                               (if (active-counter? ec)
                                   ec
                                   (make-counter (cp0-effort-limit) ctxt abort))
                               (if (prelex-source-singly-referenced? x)
                                   (passive-counter)
                                   (make-counter 
                                     (if (active-counter? sc)
                                         (counter-value sc)
                                         (cp0-size-limit))
                                     ctxt abort))))))
                       (lambda () (set-operand-outer-pending! opnd #f))))
                (residualize-ref x sc))])]
        [(primref p)
         (context-case ctxt
           [(value) rhs]
           [(test) `(const #t)]
           [(app) (fold-prim p ctxt sc)])]
        [else (residualize-ref x sc)])))
        
  (define (cp0-single-ref x opnd ctxt env ec sc)
    (and (app? ctxt)
         (let loop ((x x)(opnd opnd))
           (and (prelex-source-singly-referenced? x)
                (not (prelex-source-assigned? x))
                (exp-case (operand-expr opnd)
                  [(lambda (z) body)
                    (let ((sc (passive-counter)))
                      (struct-case opnd
                        [(operand expr env ec)
                          (let ((result (inline expr ctxt env ec sc)))
                            (set-operand-single-reference-size! opnd (passive-counter-value sc))
                            result)]))]
                  [(ref z)
                    (let ((new-z (lookup z (operand-env opnd))))
                      (and (not (eq? z new-z))
                           (loop new-z (prelex-operand new-z))))]
                  [else #f])))))                                
        
  (define (inline proc ctxt env ec sc)
    (exp-case proc
      [(lambda (x) body)
        (let ((opnd (app-opnd ctxt)))
          (with-extended-env ((env x) (env x opnd))
            (let ((body (cp0 body (app-ctxt ctxt) env ec sc)))
              (let ((result (make-let-binding x opnd body sc)))
                (set-app-inlined! ctxt #t)
                result))))]))
                
  (define (make-let-binding x opnd body sc)
    (let ((effect-opnd (lambda (result)
                         (set-operand-residualize-for-effect! opnd #t)
                         result)))
      (exp-case body
        [(const c) 
          (effect-opnd body)]
        [(primref p)
          (effect-opnd body)]
        [(ref y)
          (if (eq? x y)
              (score-value-visit-operand! opnd sc)
              (effect-opnd body))]
        [else
          (cond
            [(prelex-residual-referenced? x)
              `(call (lambda (,x) ,body) ,(score-value-visit-operand! opnd sc))]
            [(prelex-residual-assigned? x)
              (effect-opnd '(call (lambda (,x) ,body) (const ,(void))))]
            [else
              (effect-opnd body)])])))
                  
  (define (fold-prim p ctxt sc)
    (define (get-value p a)
      (call/cc
        (lambda (k)
          (with-exception-handler 
            (lambda (con) 
              (k #f))
            (lambda () 
              `(const ,((system-value p) a)))))))
    (let ([opnd (app-opnd ctxt)])
      (let ([info (primitive-info p (operand-value opnd))])
        (let ([result
          (and info 
               (or (and (effect-free? info)
                        (context-case (app-ctxt ctxt)
                          [(effect) `(const ,(void))]
                          [(test) 
                           (cond
                             [(result-true? info)
                              `(const #t)]
                             [(result-false? info)
                              `(const #f)]                                 
                             [else #f])]
                          [else #f]))
                   (and (foldable? info)
                        (let ([val (value-visit-operand! opnd)])
                          (exp-case (result-exp val)
                            [(const c)
                             (get-value info c)]
                            [else #f])))))])
              (if result
                  (begin
                    (set-operand-residualize-for-effect! opnd #t)
                    (set-app-inlined! ctxt #t)
                    result)
                  (begin
                    (decrement sc 1)
                    `(primref ,p)))))))

  (define (residualize-ref id sc)
    (decrement sc 1)
    (when (prelex-residual-referenced? id)
      (set-prelex-residual-singly-referenced?! id #f))
    (set-prelex-residual-referenced?! id #t)
    `(ref ,id))
    
  (define (cp0-optimize expr)
    (cp0 expr 'value '() (passive-counter) (passive-counter)))
    
  (define (lookup x env)
    (cond
      [(vector? env)
       (let ([lhs* (vector-ref env 0)] [rhs* (vector-ref env 1)])
         (cond
           [(eq? x lhs*) rhs*]
           [else (lookup x (vector-ref env 2))]))]
      [else x]))  

  ;;;
  (define (copy-var x)
    (let ([y (make-prelex (prelex-name x) #f)])
      (set-prelex-source-referenced?! y
        (prelex-source-referenced? x))
      (set-prelex-source-assigned?! y
        (prelex-source-assigned? x))
      (set-prelex-source-singly-referenced?! y
        (prelex-source-singly-referenced? x))
      (let ([loc (prelex-global-location x)])
        (when loc
          (set-prelex-global-location! y loc)
          (set-prelex-source-referenced?! y #t)
          (set-prelex-residual-referenced?! y #t)))
      y))

  (define (extend env lhs* rands) 
    (if (null? lhs*)
        (values env '())
        (let ([nlhs* (copy-var lhs*)])
          (when rands 
             (set-prelex-operand! nlhs* rands))
          (values (vector lhs* nlhs* env) nlhs*))))

  (define (copy-back x)
    (set-prelex-source-assigned?! x 
       (prelex-residual-assigned? x))
    (set-prelex-source-referenced?! x 
       (prelex-residual-referenced? x))
    (set-prelex-source-singly-referenced?! x
       (prelex-residual-singly-referenced? x)))

  (define-syntax with-extended-env
    (syntax-rules ()
      [(_ ((e2 args2) (e1 args1 rands)) b b* ...)
       (let-values ([(e2 args2) (extend e1 args1 rands)])
         (let ([v (let () b b* ...)])
           (copy-back args2)
           v))]))    
)