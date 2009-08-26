#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

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
    (map node-key (reverse (node-children node))))

  (define (generate-node node ids)
    (with-syntax ((pred (node-key node))
                  (id   (car ids)))
      (let ((val (node-value node)))
        (if val
            #`(and (pred id) #,val)
            (with-syntax (((c ...) (map (lambda (x)
                                          (generate-node x (cdr ids)))
                                        (reverse (node-children node))))
                          (child-keys (get-child-keys node))
                          (next-id (car (cdr ids))))
              #'(and (pred id)
                     (or
                        c ...)))))))
  (define (generate-tree tree ids else-expr)
    (with-syntax (((c ...) (map (lambda (x)
                                  (generate-node x ids))
                                (reverse (node-children tree))))
                  (else-expr else-expr))
      #'(or
          c ...
          else-expr)))
            
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
