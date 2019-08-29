#!r6rs
;;; bbtrees.sls --- Bounded Balance trees

;; Copyright (C) 2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;; Documentation:
;;
;; Note: For all procedures which take a key as an argument, the key
;; must be comparable with the ordering procedure of the bbtree.
;;
;; make-bbtree : (any -> any -> boolean) -> bbtree
;; returns an empty bbtree. bbtrees derived from this one will use the
;; procedure argument for ordering keys.
;;
;; bbtree? : any -> bool
;; returns #t if the argument is a bbtree, #f otherwise
;;
;; bbtree-size : bbtree -> non-negative integer
;; returns the number of elements in a bbtree
;;
;; bbtree-ref : bbtree any [any] -> any
;; returns the value associated with the key in the bbtree.  If the
;; value is not in the tree, then, if the optional third argument is
;; passed, it is returned, otherwise an &assertion-violation condition
;; is raised.
;;
;; bbtree-set : bbtree any any -> bbtree
;; returns a new bbtree with the key associated with the value. If the
;; key is already in the bbtree, its associated value is replaced with
;; the new value in the returned bbtree.
;;
;; bbtree-update : bbtree any (any -> any) any -> bbtree
;; returns a new bbtree with the value associated with the key updated
;; according to the update procedure. If the key was not already in
;; the bbtree, the update procedure is called on the default value,
;; and the association is added to the bbtree.
;;
;; bbtree-delete : bbtree any -> bbtree
;; returns a new bbtree with the key and its associated value
;; removed. If the key is not in the bbtree, the returned bbtree is a
;; copy of the original
;;
;; bbtree-contains? : bbtree any -> boolean
;; returns #t if there is association for key in the bbtree, false
;; otherwise
;;
;; bbtree-traverse : (any any (any -> any) (any -> any) any) any bbtree -> any
;; A general tree traversal procedure. Returns the value of applying
;; the traverser procedure to the current node's key, value, a
;; procedure to traverse the left subtree, a procedure to traverse the
;; right subtree, and a base value. The subtree traversal procedures
;; both take a base argument, and call bbtree-traverse recursively on
;; the appropriate subtree. It is mostly useful for implementing
;; other, more specific tree traversal procedures. For example,
;;   (define (l-to-r-pre-order cons base bbtree)
;;     (bbtree-traverse (lambda (key value left right base)
;;                        (right (left (cons key value base))))
;;                      base
;;                      bbtree))
;; implements a left-to-right pre-order traversal variant of bbtree-fold
;;
;; bbtree-fold : (any any any -> any) any bbtree -> any
;; returns the value obtained by the iterating the combine procedure
;; over each node in the tree. The combine procedure takes three
;; arguments, the key and value of the current node, and an
;; accumulator value, and its return value is used as the accumulator
;; value for the next node. The initial accumulator value is provided
;; by the base argument. bbtree-fold performs an left-to-right
;; in-order traversal or "minimum key to maximum key".
;;
;; bbtree-fold-right : (any any any -> any) any bbtree -> any
;; like bbtree-fold, but it performs a right-to-left in-order
;; traversal instead (i.e. maximum to minimum).
;;
;; bbtree-map : (any -> any) bbtree -> bbtree
;; returns the tree obtained by updating the value of each node with
;; the result of applying the procedure to its value.
;;
;; bbtree->alist : bbtree -> Listof(Pairs)
;; returns the key value associations of the bbtree as a list of
;; pairs. The list returned is in sorted order according to the
;; ordering procedure of the bbtree. A consequence of this is that one
;; could write a sort procedure for lists of pairs as
;;   (define (alist-sort alist <)
;;     (bbtree->alist (alist->bbtree alist <)))
;;
;; alist->bbtree : Listof(Pairs) -> (any any -> boolean) -> bbtree
;; returns the bbtree containing each of the key value pairs in the
;; alist, using the < argument as the ordering procedure.
;;
;; bbtree-keys : bbtree -> Listof(any)
;; returns a list containing all the keys of the bbtree. The keys are
;; sorted according to the bbtree's ordering procedure.
;;
;; bbtree-union : bbtree bbtree -> bbtree
;; returns a bbtree containing the union of the associations in
;; bbtree1 and bbtree2. Where the same key occurs in both, the value
;; in bbtree1 is preferred.
;; 
;; bbtree-difference : bbtree bbtree -> bbtree
;; returns a bbtree containing the all the associations in bbtree1,
;; which do not occur in bbtree2.
;;
;; bbtree-intersection : bbtree bbtree -> bbtree
;; returns a bbtree containing all the associations which appear in
;; both bbtree1 and bbtree2. The value in bbtree1 are preferred over
;; those in bbtree2.
;;
;; bbtree-index bbtree any -> non-negative integer
;; returns the index of the key in the bbtree. Index is an integer
;; between 0 and size - 1, with the a key having a lower index than
;; another if first-key < second-key, according to the bbtree ordering
;; procedure.
;;
;; bbtree-ref/index bbtree non-negative-integer -> any any
;; returns the key and value of the association in the bbtree at the
;; given index.
;;
;; bbtree-ordering-procedure : bbtree -> (any any -> bool)
;; returns the ordering procedure used internally to order the
;; bbtree.
(library (pfds bbtrees)
(export make-bbtree
        bbtree?
        bbtree-size
        bbtree-ref
        bbtree-set
        bbtree-update
        bbtree-delete
        bbtree-contains?
        bbtree-ordering-procedure
        bbtree-traverse
        bbtree-fold
        bbtree-fold-right
        bbtree-map
        bbtree->alist
        alist->bbtree
        bbtree-keys
        bbtree-union
        bbtree-difference
        bbtree-intersection
        bbtree-index
        bbtree-ref/index
        )

(import (except (rnrs) min member))

(define weight 4)

;;; bbtree is the wrapper that you interact with from outside the
;;; module, so there is no need to deal with empty and node record types
(define-record-type (bbtree %make-bbtree bbtree?)
  (fields tree ordering-procedure))

(define (update-tree bbtree new-tree)
  (%make-bbtree new-tree (bbtree-ordering-procedure bbtree)))

;;; inner representation of trees
;;; all non exposed methods can assume a valid tree
(define-record-type empty)

(define-record-type node
  (fields key value length left right))

;;; smart constructor for nodes, automatically fills in size field
(define (node* key value left right)
  (make-node key value (+ 1 (size left) (size right)) left right))

(define (size tree)
  (if (empty? tree)
      0
      (node-length tree)))

;; looks key up in the tree, and applies proc to the value if it finds
;; it, and calls failure otherwise
(define (lookup tree key proc failure <)
  (define (search tree)
    (cond [(empty? tree) (failure)]
          [(< (node-key tree) key)
           (search (node-right tree))]
          [(< key (node-key tree))
           (search (node-left tree))]
          [else (proc tree)]))
  (search tree))

;; returns the key and value of the minimum element in the tree
(define (min tree)
  (cond [(empty? tree)
         (assertion-violation 'min "Can't take the minimum value of an empty tree")]
        [(empty? (node-left tree))
         (values (node-key tree)
                 (node-value tree))]
        [else
         (min (node-left tree))]))

;;; rotations
(define (rotate-left key value left right)
  (let ([r-key   (node-key right)]
        [r-value (node-value right)]
        [r-left  (node-left right)]
        [r-right (node-right right)])
    (node* r-key
           r-value
           (node* key value left r-left)
           r-right)))

(define (rotate-right key value left right)
  (let ([l-key   (node-key left)]
        [l-value (node-value left)]
        [l-left  (node-left left)]
        [l-right (node-right left)])
    (node* l-key
           l-value
           l-left
           (node* key value l-right right))))

(define (rotate-left/double key value left right)
  (let ([r-key   (node-key right)]
        [r-value (node-value right)]
        [r-left  (node-left right)]
        [r-right (node-right right)])
    (let ([rl-key   (node-key r-left)]
          [rl-value (node-value r-left)]
          [rl-left  (node-left r-left)]
          [rl-right (node-right r-left)])
      (node* rl-key
             rl-value
             (node* key value left rl-left)
             (node* r-key r-value rl-right r-right)))))

(define (rotate-right/double key value left right)
  (let ([l-key   (node-key left)]
        [l-value (node-value left)]
        [l-left  (node-left left)]
        [l-right (node-right left)])
    (let ([lr-key   (node-key l-right)]
          [lr-value (node-value l-right)]
          [lr-left  (node-left l-right)]
          [lr-right (node-right l-right)])
      (node* lr-key
             lr-value
             (node* l-key l-value l-left lr-left)
             (node* key value lr-right right)))))

;;; smart constructor for after adding/removing a node
(define (T key value left right)
  (let ((l-size (size left))
        (r-size (size right)))
    (cond [(< (+ l-size r-size) 2)
           (node* key value left right)]
          [(> r-size (* weight l-size))
           (let ([r-left (node-left right)]
                 [r-right (node-right right)])
             (if (< (size r-left) (size r-right))
                 (rotate-left key value left right)
                 (rotate-left/double key value left right)))]
          [(> l-size (* weight r-size))
           (let ([l-left (node-left left)]
                 [l-right (node-right left)])
             (if (< (size l-right) (size l-left))
                 (rotate-right key value left right)
                 (rotate-right/double key value left right)))]
          [else
           (node* key value left right)])))

(define (update tree key proc default <)
  (define (add-to tree)
    (if (empty? tree)
        (make-node key (proc default) 1 (make-empty) (make-empty))
        (let ([k (node-key tree)]
              [v (node-value tree)]
              [l (node-left tree)]
              [r (node-right tree)])
          (cond [(< key k)
                 (T k v (add-to l) r)]
                [(< k key)
                 (T k v l (add-to r))]
                [else
                 (node* key (proc v) l r)]))))
  (add-to tree))

(define (add tree key value <)
  (define (replace _) value)
  (update tree key replace #f <))

(define (delete tree key <)
  (define (delete-from tree)
    (if (empty? tree)
        tree
        (let ([k (node-key tree)]
              [v (node-value tree)]
              [l (node-left tree)]
              [r (node-right tree)])
          (cond [(< key k)
                 (T k v (delete-from l) r)]
                [(< k key)
                 (T k v l (delete-from r))]
                [else
                 (delete* l r)]))))
  (delete-from tree))

(define (delete* left right)
  (cond ((empty? left) right)
        ((empty? right) left)
        (else
         (let-values (((k v) (min right)))
           (T k v left (delete-min right))))))

(define (delete-min tree)
  (cond ((empty? tree)
         (assertion-violation 'delete-min
                              "Can't delete the minimum value of an empty tree"))
        ((empty? (node-left tree))
         (node-right tree))
        (else
         (T (node-key tree)
            (node-value tree)
            (delete-min (node-left tree))
            (node-right tree)))))

(define (concat3 key value left right lt)
  (cond [(empty? left)
         (add right key value lt)]
        [(empty? right)
         (add left key value lt)]
        [(< (* weight (size left)) (size right))
         (T (node-key right)
            (node-value right)
            (concat3 key value left (node-left right) lt)
            (node-right right))]
        [(< (* weight (size right)) (size left))
         (T (node-key left)
            (node-value left)
            (node-left left)
            (concat3 key value (node-right left) right lt))]
        [else
         (node* key value left right)]))

(define (split-lt tree key <)
  (cond [(empty? tree) tree]
        [(< key (node-key tree))
         (split-lt (node-left tree) key <)]
        [(< (node-key tree) key)
         (concat3 (node-key tree)
                  (node-value tree)
                  (node-left tree)
                  (split-lt (node-right tree) key <)
                  <)]
        [else (node-left tree)]))

(define (split-gt tree key <)
  (cond [(empty? tree) tree]
        [(< key (node-key tree))
         (concat3 (node-key tree)
                  (node-value tree)
                  (split-gt (node-left tree) key <)
                  (node-right tree)
                  <)]
        [(< (node-key tree) key)
         (split-gt (node-right tree) key <)]
        [else (node-right tree)]))

(define (difference tree1 tree2 <)
  (cond [(empty? tree1) tree1]
        [(empty? tree2) tree1]
        [else
         (let ([l* (split-lt tree1 (node-key tree2) <)]
               [r* (split-gt tree1 (node-key tree2) <)])
           (concat (difference l* (node-left tree2) <)
                   (difference r* (node-right tree2) <)))]))

(define (concat left right)
  (cond [(empty? left) right]
        [(empty? right) left]
        [(< (* weight (size left)) (size right))
         (T (node-key right)
            (node-value right)
            (concat left (node-left right))
            (node-right right))]
        [(< (* weight (size right)) (size left))
         (T (node-key left)
            (node-value left)
            (node-left left)
            (concat (node-right left) right))]
        [else
         (let-values (((k v) (min right)))
           (T k v left (delete-min right)))]))

(define (member key tree <)
  (define (yes x) #t)
  (define (no) #f)
  (lookup tree key yes no <))

(define (intersection t1 t2 <)
  (cond [(empty? t1) t1]
        [(empty? t2) t2]
        [else
         (let ([l* (split-lt t2 (node-key t1) <)]
               [r* (split-gt t2 (node-key t1) <)])
           (if (member (node-key t1) t2 <)
               (concat3 (node-key t1)
                        (node-value t1)
                        (intersection (node-left t1) l* <)
                        (intersection (node-right t1) r* <)
                        <)
               (concat (intersection (node-left t1) l* <)
                       (intersection (node-right t1) r* <))))]))

;;; hedge union

;; ensures that tree is either empty, or root lies in range low--high
(define (trim low high tree <)
  (cond [(empty? tree) tree]
        [(< low (node-key tree))
         (if (< (node-key tree) high)
             tree
             (trim low high (node-left tree) <))]
        [else
         (trim low high (node-right tree) <)]))

(define (uni-bd tree1 tree2 low high <)
  (cond [(empty? tree2) tree1]
        [(empty? tree1)
         (concat3 (node-key tree2)
                  (node-value tree2)
                  (split-gt (node-left tree2) low <)
                  (split-lt (node-right tree2) high <)
                  <)]
        [else
         (let ([key (node-key tree1)])
           (concat3 key
                    (node-value tree1)
                    (uni-bd (node-left tree1) (trim low key tree2 <) low key <)
                    (uni-bd (node-right tree1) (trim key high tree2 <) key high <)
                    <))]))

;; specialisation of trim for high=+infinity
(define (trim-low low tree <)
  (cond [(empty? tree) tree]
        [(< low (node-key tree)) tree]
        [else
         (trim-low low (node-right tree) <)]))

;; trim for low=-infinity
(define (trim-high high tree <)
  (cond [(empty? tree) tree]
        [(< (node-key tree) high) tree]
        [else
         (trim-high high (node-left tree) <)]))

;; uni-bd for low=-infinity
(define (uni-high tree1 tree2 high <)
  (cond [(empty? tree2) tree1]
        [(empty? tree1)
         (concat3 (node-key tree2)
                  (node-value tree2)
                  (node-left tree2)
                  (split-lt (node-right tree2) high <)
                  <)]
        [else
         (let ([key (node-key tree1)])
           (concat3 key
                    (node-value tree1)
                    (uni-high (node-left tree1) (trim-high key tree2 <) key <)
                    (uni-bd (node-right tree1) (trim key high tree2 <) key high <)
                    <))]))

;; uni-bd for high=+infinity
(define (uni-low tree1 tree2 low <)
  (cond [(empty? tree2) tree1]
        [(empty? tree1)
         (concat3 (node-key tree2)
                  (node-value tree2)
                  (split-gt (node-left tree2) low <)
                  (node-right tree2)
                  <)]
        [else
         (let ([key (node-key tree1)])
           (concat3 key
                    (node-value tree1)
                    (uni-bd (node-left tree1) (trim low key tree2 <) low key <)
                    (uni-low (node-right tree1) (trim-low key tree2 <) key <)
                    <))]))

(define (hedge-union tree1 tree2 <)
  (cond [(empty? tree2) tree1]
        [(empty? tree1) tree2]
        [else
         (let ([key (node-key tree1)])
           (concat3 key
                    (node-value tree1)
                    (uni-high (node-left tree1) (trim-high key tree2 <) key <)
                    (uni-low (node-right tree1) (trim-low key tree2 <) key <)
                    <))]))

;;; rank and indexing

(define (rank tree key <)
  (cond [(empty? tree);; error
         (assertion-violation 'rank "Key is not in the tree" key)]
        [(< key (node-key tree))
         (rank (node-left tree) key <)]
        [(< (node-key tree) key)
         (+ (rank (node-right tree) key <)
            (size (node-left tree))
            1)]
        [else
         (size (node-left tree))]))

(define (index tree idx)
  (if (empty? tree)
      (assertion-violation 'index "No value at index" idx)
      (let ([l-size (size (node-left tree))])
        (cond [(< idx l-size)
               (index (node-left tree) idx)]
              [(< l-size idx)
               (index (node-right tree)
                      (- idx l-size 1))]
              [else
               (values (node-key tree)
                       (node-value tree))]))))

;;; External procedures

(define (make-bbtree <)
  (assert (procedure? <))
  (%make-bbtree (make-empty) <))

(define (bbtree-size bbtree)
  (assert (bbtree? bbtree))
  (size (bbtree-tree bbtree)))

(define bbtree-ref
  (let ((ref (lambda (bbtree key failure)
               (assert (bbtree? bbtree))
               (lookup (bbtree-tree bbtree)
                       key
                       node-value
                       failure
                       (bbtree-ordering-procedure bbtree)))))
    (case-lambda
      ((bbtree key)
       (define (fail)
         (assertion-violation 'bbtree-ref "Key is not in the tree" key))
       (ref bbtree key fail))
      ((bbtree key ret)
       (ref bbtree key (lambda () ret))))))

(define (bbtree-set bbtree key value)
  (assert (bbtree? bbtree))
  (update-tree bbtree
               (add (bbtree-tree bbtree)
                    key
                    value
                    (bbtree-ordering-procedure bbtree))))

(define (bbtree-update bbtree key proc default)
  (assert (bbtree? bbtree))
  (update-tree bbtree
               (update (bbtree-tree bbtree)
                       key
                       proc
                       default
                       (bbtree-ordering-procedure bbtree))))

(define (bbtree-delete bbtree key)
  (assert (bbtree? bbtree))
  (update-tree bbtree
               (delete (bbtree-tree bbtree)
                       key
                       (bbtree-ordering-procedure bbtree))))

(define (bbtree-contains? bbtree key)
  (assert (bbtree? bbtree))
  (lookup (bbtree-tree bbtree)
          key
          (lambda (_) #t)
          (lambda () #f)
          (bbtree-ordering-procedure bbtree)))

;; iterators

(define (traverse traverser base tree)
  (define (left base)
    (traverse traverser base (node-left tree)))
  (define (right base)
    (traverse traverser base (node-right tree)))
  (if (empty? tree)
      base
      (traverser (node-key tree)
                 (node-value tree)
                 left
                 right
                 base)))

(define (bbtree-traverse traverser base bbtree)
  (assert (bbtree? bbtree))
  (traverse traverser base (bbtree-tree bbtree)))

(define (bbtree-fold combine base bbtree)
  (assert (bbtree? bbtree))
  (traverse (lambda (k v l r n)
              (r (combine k v (l n))))
            base
            (bbtree-tree bbtree)))

(define (bbtree-fold-right combine base bbtree)
  (assert (bbtree? bbtree))
  (traverse (lambda (k v l r n)
              (l (combine k v (r n))))
            base
            (bbtree-tree bbtree)))

;; I could do this more efficiently, but is it worth it?
(define (bbtree-map mapper bbtree)
  (bbtree-fold (lambda (key value tree)
                 (bbtree-set tree key (mapper value)))
               (make-bbtree (bbtree-ordering-procedure bbtree))
               bbtree))

(define (alist-cons a b c)
  (cons (cons a b) c))

(define (bbtree->alist bbtree)
  (bbtree-fold-right alist-cons '() bbtree))

(define (alist->bbtree list <)
  (fold-left (lambda (tree kv-pair)
               (bbtree-set tree (car kv-pair) (cdr kv-pair)))
             (make-bbtree <)
             list))

(define (bbtree-keys bbtree)
  (bbtree-fold-right (lambda (key value base)
                       (cons key base))
                     '()
                     bbtree))

(define (bbtree-union bbtree1 bbtree2)
  (update-tree bbtree1
               (hedge-union (bbtree-tree bbtree1)
                            (bbtree-tree bbtree2)
                            (bbtree-ordering-procedure bbtree1))))

(define (bbtree-difference bbtree1 bbtree2)
  (update-tree bbtree1
               (difference (bbtree-tree bbtree1)
                           (bbtree-tree bbtree2)
                           (bbtree-ordering-procedure bbtree1))))

(define (bbtree-intersection bbtree1 bbtree2)
  (update-tree bbtree1
               (intersection (bbtree-tree bbtree1)
                             (bbtree-tree bbtree2)
                             (bbtree-ordering-procedure bbtree1))))

(define (bbtree-index bbtree key)
  ;; maybe this should return #f instead of throwing an exception?
  (assert (bbtree? bbtree))
  (rank (bbtree-tree bbtree)
        key
        (bbtree-ordering-procedure bbtree)))

(define (bbtree-ref/index bbtree idx)
  (assert (bbtree? bbtree))
  (let ((tree (bbtree-tree bbtree)))
    (unless (and (integer? idx)
                 (<= 0 idx (- (size tree) 1)))
      (assertion-violation 'bbtree-ref/index
                           "Not a valid index into the bbtree"
                           idx))
    (index tree idx)))

)
