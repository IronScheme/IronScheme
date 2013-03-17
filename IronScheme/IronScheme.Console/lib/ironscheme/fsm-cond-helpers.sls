#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme fsm-cond-helpers)
  (export
    get-predicates
    fsm-cond-transformer)
  (import
    (ironscheme))
    
  (define sort-proc (make-parameter #f))

  (define-record-type node
    (protocol (lambda (p)
                (lambda (key)
                  (p key #f '()))))
    (fields key
            (mutable value)
            (mutable children)))

  (define (get-predicates preds)
    (let ((ht (make-bound-id-hashtable)))
      (for-each 
        (lambda (x)
          (hashtable-set! ht x x))
        preds)
      (hashtable-keys ht)))

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
    (if (null? keys)
        (node-value-set! tree value)
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
                (f (cdr keys) cnode))))))

  (define (get-child-keys node)
    (map node-key (reverse (node-children node))))
    
  (define (get-sorted-children node)
    (let ((sp (sort-proc))
          (nc (node-children node)))
      (if sp
          (list-sort (lambda (x y) 
                       (sp (syntax->datum x)
                           (syntax->datum y))) 
                     nc)
          (reverse nc))))

  (define (generate-node node ids)
    (with-syntax ((pred (node-key node))
                  (id   (car ids)))
      (let ((val (node-value node)))
        (if val
            #`(and (pred id) (lambda () #,val))
            (with-syntax (((c ...) (map (lambda (x)
                                          (generate-node x (cdr ids)))
                                        (get-sorted-children node)))
                          (child-keys (get-child-keys node))
                          (next-id (car (cdr ids))))
              #'(and (pred id)
                     (or c ... #f)))))))
                        
  (define (generate-tree tree ids else-expr)
    (with-syntax (((c ...) (map (lambda (x)
                                  (generate-node x ids))
                                (get-sorted-children tree)))
                  (zero (let ((nv (node-value tree)))
                          (or nv #'#f)))
                  (else-expr else-expr))
      #'(let ((r (or zero c ... #f)))
          (if r
              (r)
              else-expr))))
              
  (define (fsm-cond-transformer proc)
    (lambda (x)
      (syntax-case x (else)
        [(_ (id ...) ((pred ...) expr) ... (else else-expr))
          (for-all identifier? #'(pred ... ... id ...))
          (parameterize [(sort-proc proc)]
            (let* ((tree (make-tree)))
              (for-each (lambda (preds expr)
                          (tree-add! tree expr preds))
                        #'((pred ...) ...)
                        #'(expr ...))
              (generate-tree tree #'(id ...) #'else-expr)))]
        [(k (id ...) ((pred ...) expr) ...)
          #'(k (id ...) ((pred ...) expr) ... (else #f))])))              

  ;; analysis, todo
  (define (make-bound-id-hashtable)
    (make-hashtable (lambda (x) (symbol-hash (syntax->datum x))) bound-identifier=?))
    
  (define (analyze-predicates clauses) 
    (let ((ht (make-eq-hashtable)))
      (for-each (lambda (clause)
                  (syntax-case clause ()
                    [(pred id)
                      (hashtable-update! ht #'id 
                         (lambda (v)
                           (hashtable-update! v #'pred 
                              (lambda (v) (fx+ v 1)) 0)
                           v)
                         (make-bound-id-hashtable))]))
                clauses)
      (hashtable-for-each ht
        (lambda (k v)
          (printf "~s :\n" k)
          (hashtable-for-each v 
            (lambda (k v)
              (printf "~s => ~s\n" k v)))))
      ht))
      
  (define (get-max ht id)
    (let* ((preds (hashtable-ref ht id #f))
           (lens  (hashtable-map preds (lambda (k v) v))))
      (apply max lens)))
      
      
  (define (sort-ids ids ht)
    (list-sort (lambda (id1 id2)
                 (let ((max1 (get-max ht id1))
                       (max2 (get-max ht id2)))
                   (fx>? max1 max2)))
               ids)))
