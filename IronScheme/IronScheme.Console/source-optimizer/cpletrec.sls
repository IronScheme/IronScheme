(library (source-optimizer cpletrec)
  (export 
    (rename optimize-letrec/scc cpletrec))
  (import 
    (ironscheme)
    (source-optimizer prelex)
    (as-match))

  (define (optimize-letrec/scc x)
    (define who 'optimize-letrec/scc)
    (module (get-sccs-in-order)
      (define-struct node (data link* lowlink root done collection))
      (define (create-graph v* e** data*)
        (define h (make-eq-hashtable))
        (let ([v*
               (let f ([v* v*] [data* data*])
                 (cond
                   [(null? v*) '()]
                   [else
                    (let ([node (make-node (car data*) '() #f #f #f #f)])
                      (hashtable-set! h (car v*) node) 
                      (cons node (f (cdr v*) (cdr data*))))]))])
          (for-each
            (lambda (v e*)
              (set-node-link*! v
                (map (lambda (f) 
                       (or (hashtable-ref h f #f)
                           (error who "invalid node" f)))
                     e*)))
            v* e**)
          v*))
      (define (compute-sccs v*) ; Tarjan's algorithm
        (define scc* '())
        (define (compute-sccs v)
          (define index 0)
          (define stack '())
          (define (tarjan v)
            (let ([v-index index])
              (set-node-root! v v-index)
              (set! stack (cons v stack))
              (set! index (fx+ index 1))
              (for-each
                (lambda (v^)
                  (unless (node-done v^)
                    (unless (node-root v^) (tarjan v^))
                    (set-node-root! v (fxmin (node-root v) (node-root v^)))))
                (node-link* v))
              (when (fx= (node-root v) v-index)
                (set! scc*
                  (cons
                    (let f ([ls stack])
                      (let ([v^ (car ls)])
                        (set-node-done! v^ #t)
                        (cons v^ (if (eq? v^ v)
                                     (begin (set! stack (cdr ls)) '())
                                     (f (cdr ls))))))
                    scc*)))))
          (tarjan v))
        (for-each (lambda (v) (unless (node-done v) (compute-sccs v))) v*)
        (reverse scc*))
      (define (get-sccs-in-order n* e** data*)
        (let ([G (create-graph n* e** data*)])
          (let ([sccs (compute-sccs G)])
            (map (lambda (scc) (map node-data scc)) sccs)))))
    (define (gen-letrecs scc* ordered? body) 
      (define (mkfix b* body)
        (if (null? b*) 
            body
            (make-fix (map binding-lhs b*) 
                      (map binding-rhs b*) 
                      body)))
      (define (gen-letrec scc fix* body)
        (define (mklet lhs* rhs* body)
          (if (null? lhs*) 
              body
              (make-bind lhs* rhs* body)))
        (define (lambda-binding? x)
          (and (not (prelex-source-assigned? (binding-lhs x)))
               (clambda? (binding-rhs x))))
        (define (mkset!s b* body)
          (cond
            [(null? b*) body]
            [else 
             (let* ([b (car b*)]
                    [lhs (binding-lhs b)])
               (unless (prelex-source-assigned? lhs) 
                 (set-prelex-source-assigned?! lhs 
                    (or (prelex-global-location lhs) #t)))
               (make-seq
                 (make-assign lhs (binding-rhs b))
                 (mkset!s (cdr b*) body)))]))
        (cond
          [(null? (cdr scc)) 
           (let ([b (car scc)])
             (cond
               [(lambda-binding? b)
                (values (cons b fix*) body)]
               [(not (memq b (binding-free* b)))
                (values '()
                  (mklet (list (binding-lhs b))
                         (list (binding-rhs b))
                    (mkfix fix* body)))]
               [else 
                (values '()
                  (mklet (list (binding-lhs b)) 
                         (list (make-funcall (make-primref 'void) '()))
                    (mkset!s scc 
                      (mkfix fix* body))))]))]
          [else 
           (let-values ([(lambda* complex*) 
                         (partition lambda-binding? scc)])
             (cond
               [(null? complex*) 
                (values (append lambda* fix*) body)]
               [else
                (let ([complex* 
                       (if ordered? (sort-bindings complex*) complex*)])
                  (values '()
                    (mklet (map binding-lhs complex*)
                           (map (lambda (x)
                                  (make-funcall (make-primref 'void) '()))
                                 complex*)
                       (mkfix (append lambda* fix*)
                         (mkset!s complex* body)))))]))]))
      (let-values ([(fix* body)
                    (let f ([scc* scc*])
                      (cond
                        [(null? scc*) (values '() body)]
                        [else 
                         (let-values ([(fix* body) (f (cdr scc*))])
                           (gen-letrec (car scc*) fix* body))]))])
        (mkfix fix* body)))
    (define (do-recbind lhs* rhs* body bc ordered?)
      (define (make-bindings lhs* rhs* bc i)
        (cond
          [(null? lhs*) '()]
          [else 
           (let ([b (make-binding i (car lhs*) (car rhs*) #f bc '())])
             (set-prelex-operand! (car lhs*) b)
             (cons b (make-bindings (cdr lhs*) (cdr rhs*) bc (+ i 1))))]))
      (define (complex? x) 
        (or (binding-complex x) 
            (prelex-source-assigned? (binding-lhs x))))
      (define (insert-order-edges b*)
        (define (mark pb b*)
          (unless (null? b*)
            (let ([b (car b*)])
              (if (complex? b)
                  (let ([free* (binding-free* b)])
                    (unless (memq pb free*)
                      (set-binding-free*! b (cons pb free*)))
                    (mark b (cdr b*)))
                  (mark pb (cdr b*))))))
        (unless (null? b*)
          (let ([b (car b*)])
            (if (complex? b)
                (mark b (cdr b*))
                (insert-order-edges (cdr b*))))))
      (let ([b* (make-bindings lhs* rhs* bc 0)])
        (for-each (lambda (b) (set-binding-rhs! b (E (binding-rhs b) b))) b*)
        (for-each (lambda (x) (set-prelex-operand! x #f)) lhs*)
        (let ([body (E body bc)]) 
          (when ordered? (insert-order-edges b*))
          (let ([scc* (get-sccs-in-order b* (map binding-free* b*) b*)])
            (when (debug-scc)
              (printf "SCCS:\n")
              (for-each 
                (lambda (scc) 
                  (printf "  ~s\n" 
                    (map unparse (map binding-lhs scc))))
                scc*))
            (gen-letrecs scc* ordered? body)))))
    (define (sort-bindings ls)
      (list-sort
        (lambda (x y) (< (binding-serial x) (binding-serial y)))
        ls))
    (define-struct binding (serial lhs rhs complex prev free*))
    (define (mark-complex bc)
      (unless (binding-complex bc)
        (set-binding-complex! bc #t)
        (mark-complex (binding-prev bc))))
    (define (mark-free var bc)
      (let ([rb (prelex-operand var)])
        (when rb
          (let ([lb 
                 (let ([pr (binding-prev rb)])
                   (let f ([bc bc])
                     (let ([bcp (binding-prev bc)])
                       (cond
                         [(eq? bcp pr) bc]
                         [else (f bcp)]))))])
            (let ([free* (binding-free* lb)])
              (unless (memq rb free*)
                ;(printf "MARK FREE ~s in ~s\n" 
                ;        (unparse (binding-lhs rb))
                ;        (unparse (binding-lhs lb)))
                (set-binding-free*! lb (cons rb free*))))))))
    (define (E* x* bc)
      (map (lambda (x) (E x bc)) x*))
    (define (L x bc)
      (struct-case x
        [(clambda g cls* cp free name)
         (let ([bc (make-binding #f #f #f #t bc '())])
           (make-clambda g
             (map (lambda (x)
                    (struct-case x
                      [(clambda-case info body)
                       (make-clambda-case info (E body bc))]))
                  cls*)
             cp free name))]))
    (define (E x bc)
      (struct-case x
        [(constant) x]
        [(prelex) 
         (assert (prelex-source-referenced? x))
         (mark-free x bc)
         (when (prelex-source-assigned? x)
           (mark-complex bc))
         x]
        [(assign lhs rhs)
         (assert (prelex-source-assigned? lhs))
         ;(set-prelex-source-assigned?! lhs #t)
         (mark-free lhs bc)
         (mark-complex bc)
         (make-assign lhs (E rhs bc))]
        [(primref) x]
        [(bind lhs* rhs* body)
         (if (null? lhs*)
             (E body bc)
             (make-bind lhs* (E* rhs* bc) (E body bc)))]
        [(recbind lhs* rhs* body)
         (if (null? lhs*)
             (E body bc)
             (do-recbind lhs* rhs* body bc #f))]
        [(rec*bind lhs* rhs* body)
         (if (null? lhs*)
             (E body bc)
             (do-recbind lhs* rhs* body bc #t))]
        [(conditional e0 e1 e2)
         (make-conditional (E e0 bc) (E e1 bc) (E e2 bc))]
        [(seq e0 e1) (make-seq (E e0 bc) (E e1 bc))]
        [(clambda g cls* cp free name)
         (L x bc)]
        [(funcall rator rand*)
         (mark-complex bc)
         (make-funcall (E rator bc) (E* rand* bc))]
        [(mvcall p c)
         (mark-complex bc)
         (make-mvcall (E p bc) (E c bc))]
        [(forcall rator rand*) 
         (mark-complex bc)
         (make-forcall rator (E* rand* bc))]
        [else (error who "invalid expression" (unparse x))]))
    ;(printf "===========================================\n")
    (let ([x (E x (make-binding #f #f #f #t #t '()))])
      ;(pretty-print (unparse x)) 
      x))

)