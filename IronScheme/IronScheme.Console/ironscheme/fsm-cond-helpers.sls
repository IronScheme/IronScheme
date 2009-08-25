(library (ironscheme fsm-cond-helpers)
  (export 
    make-tree
    tree-add!
    generate-tree)
  (import
    (ironscheme))
    
  (define-record-type node 
    (protocol (lambda (p)
                (lambda (key)
                  (p key #f '()))))
    (fields key 
            (mutable value) 
            (mutable children)))

  (define (get-node node key)
    (let f ((c (node-children node)))
      (if (null? c)
          #f
          (let ((node (car c)))
            (if (bound-identifier=? (node-key node) key)
                node
                (f (cdr c)))))))

  (define (make-tree)  (make-node #f))

  (define (tree-add! tree value keys)
    (let f ((keys keys)(node tree))
      (let ((cnode (or (get-node node (car keys))
                       (let ((cnode (make-node (car keys))))
                         (node-children-set! node (cons cnode (node-children node)))
                         cnode))))
        (if (null? (cdr keys))
            (let ((nv (node-value cnode)))
              (when nv
                (syntax-violation 'tree-add! "duplicate match" (list nv value) (car keys)))
              (node-value-set! cnode value))
            (f (cdr keys) cnode)))))
                
  (define (get-child-keys node)
    (map node-key (node-children node)))              
                  
  (define (generate-node node ids)
    (with-syntax ((pred (node-key node))
                  (id   (car ids)))
      (let ((val (node-value node)))
        (if val
            #`((pred id) #,val)
            (with-syntax (((c ...) (map (lambda (x)
                                          (generate-node x (cdr ids))) 
                                        (node-children node)))
                          (child-keys (get-child-keys node))                                      
                          (next-id (car (cdr ids))))
              #'((pred id)
                  (cond
                    c ...
                    ;;;
                    [else 
                      (assertion-violation #f "not matched" next-id 'child-keys)])))))))

  (define (generate-tree tree ids)
    (with-syntax (((c ...) (map (lambda (x)
                                  (generate-node x ids)) 
                                (node-children tree)))
                  (child-keys (get-child-keys tree))                              
                  (next-id (car ids)))                              
      #'(cond
          c ...
          [else 
            (assertion-violation #f "not matched" next-id 'child-keys)]))))
    