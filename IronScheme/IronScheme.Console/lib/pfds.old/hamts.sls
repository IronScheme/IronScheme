#!r6rs
;;; hamts.sls --- Hash Array Mapped Tries

;; Copyright (C) 2014 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;; Documentation:
;;
;; Note: For all procedures which take a key as an argument, the key
;; must be hashable with the hamt hash function, and comparable with
;; the hamt equivalence predicate.
;;
;; make-hamt : (any -> non-negative integer) (any -> any -> boolean) -> hamt
;; returns a new empty hamt using the given hash and equivalence functions.
;;
;; hamt? : any -> boolean
;; returns #t if argument is a hamt, #f otherwise.
;;
;; hamt-size : hamt -> non-negative integer
;; returns the number of associations in the hamt.
;;
;; hamt-ref : hamt any [any] -> any
;; returns the value associated with the key in the hamt. If there is
;; no value associated with the key, it returns the default value if
;; provided, or raises an &assertion-violation if it isn't.
;;
;; hamt-contains? : hamt any -> boolean
;; returns #t if there is an association for the key in the hamt, #f
;; otherwise.
;;
;; hamt-set : hamt any any -> hamt
;; returns a new hamt with the key associated to the value. If the key
;; is already associated with a value, it is replaced.
;;
;; hamt-update : hamt any (any -> any) any -> hamt
;; returns a new hamt with the valued associated with the key updated
;; by the update procedure. If the hamt does not already have a value
;; associated with the key, then it applies the update procedure to
;; the default value, and associates the key with that.
;;
;; hamt-delete : hamt any -> hamt
;; returns a hamt with the key and its associated value removed. If
;; the key is not in the hamt, a copy of the original hamt is
;; returned.
;;
;; hamt-fold : (any any any -> any) any hamt -> hamt
;; returns the value obtained by iterating the combine procedure over
;; each key value pair in the hamt. The combine procedure takes three
;; arguments, the key and value of an association, and an accumulator,
;; and returns a new accumulator value. The initial value of the
;; accumulator is provided by the base argument. The order in which
;; the hamt is traversed is not guaranteed.
;;
;; hamt-map : (any -> any) hamt -> hamt
;; returns the hamt obtained by applying the update procedure to each
;; of the values in the hamt.
;;
;; hamt->alist : hamt -> Listof(Pairs)
;; returns the key/value associations of the hamt as a list of pairs.
;; The order of the list is not guaranteed.
;;
;; alist->hamt : Listof(Pairs) (any -> non-negative integer) (any -> any -> boolean) -> hamt
;; returns the hamt containing the associations specified by the pairs
;; in the alist. If the same key appears in the alist multiple times,
;; its leftmost value is the one that is used.
;;
;; hamt-equivalence-predicate : hamt -> (any -> any -> boolean)
;; returns the procedure used internally by the hamt to compare keys.
;;
;; hamt-hash-function : hamt -> (any -> non-negative integer)
;; returns the hash procedure used internally by the hamt.
;;
(library (pfds hamts)
(export make-hamt
        hamt?
        hamt-size
        hamt-ref
        hamt-set
        hamt-update
        hamt-delete
        hamt-contains?
        hamt-equivalence-predicate
        hamt-hash-function
        hamt-fold
        hamt-map
        hamt->alist
        alist->hamt
        )
(import (rnrs)
        (pfds private vectors)
        (pfds private alists)
        (pfds private bitwise))

;;; Helpers

(define cardinality 32) ; 64

(define (mask key level)
  (bitwise-arithmetic-shift-right (bitwise-and key (- (expt 2 5) 1)) level))

(define (level-up level)
  (+ level 5))

(define (ctpop key index)
  (bitwise-bit-count (bitwise-arithmetic-shift-right key (+ 1 index))))

;;; Node types

(define-record-type (subtrie %make-subtrie subtrie?)
  (fields size bitmap vector))

(define (make-subtrie bitmap vector)
  (define vecsize
    (vector-fold (lambda (val accum)
                   (+ (size val) accum))
                 0
                 vector))
  (%make-subtrie vecsize bitmap vector))

(define-record-type leaf
  (fields key value))

(define-record-type (collision %make-collision collision?)
  (fields size hash alist))

(define (make-collision hash alist)
  (%make-collision (length alist) hash alist))

;;; Main

(define (lookup vector key default hash eqv?)
  (define (handle-subtrie node level)
    (define bitmap (subtrie-bitmap node))
    (define vector (subtrie-vector node))
    (define index (mask h level))
    (if (not (bitwise-bit-set? bitmap index))
        default
        (let ((node (vector-ref vector (ctpop bitmap index))))
          (cond ((leaf? node)
                 (handle-leaf node))
                ((collision? node)
                 (handle-collision node))
                (else
                 (handle-subtrie node (level-up level)))))))

  (define (handle-leaf node)
    (if (eqv? key (leaf-key node))
        (leaf-value node)
        default))

  (define (handle-collision node)
    (alist-ref (collision-alist node) key default eqv?))
  
  (define h (hash key))
  (define node (vector-ref vector (mask h 0)))

  (cond ((not node) default)
        ((leaf? node) (handle-leaf node))
        ((collision? node) (handle-collision node))
        (else
         (handle-subtrie node (level-up 0)))))

(define (insert hvector key update base hash eqv?)
  (define (handle-subtrie subtrie level)
    (define bitmap (subtrie-bitmap subtrie))
    (define vector (subtrie-vector subtrie))
    (define index (mask h level))
    (define (fixup node)
      (make-subtrie bitmap (vector-set vector index node)))
    (if (not (bitwise-bit-set? bitmap index))
        (make-subtrie (bitwise-bit-set bitmap index)
                      (vector-insert vector
                                     (ctpop bitmap index)
                                     (make-leaf key (update base))))
        (let ((node (vector-ref vector (ctpop bitmap index))))
          (cond ((leaf? node)
                 (fixup (handle-leaf node level)))
                ((collision? node)
                 (fixup (handle-collision node level)))
                (else
                 (fixup (handle-subtrie node (level-up level))))))))

  (define (handle-leaf node level)
    (define lkey  (leaf-key node))
    (define khash (bitwise-arithmetic-shift-right h level))
    (define lhash (bitwise-arithmetic-shift-right (hash lkey) level))
    (cond ((eqv? key lkey)
           (make-leaf key (update (leaf-value node))))
          ((equal? khash lhash)
           (make-collision lhash
                           (list (cons lkey (leaf-value node))
                                 (cons key (update base)))))
          (else
           (handle-subtrie (wrap-subtrie node lhash) (level-up level)))))

  (define (handle-collision node level)
    (define khash (bitwise-arithmetic-shift-right h level))
    (define chash (bitwise-arithmetic-shift-right (collision-hash node) level))
    (if (equal? khash chash)
        (make-collision (collision-hash node)
                        (alist-update (collision-alist node) key update base eqv?))
        ;; TODO: there may be a better (more efficient) way to do this
        ;; but simple is better for now (see also handle-leaf)
        (handle-subtrie (wrap-subtrie node chash) (level-up level))))

  (define (wrap-subtrie node chash)
    (make-subtrie (bitwise-bit-set 0 (mask chash 0)) (vector node)))

  (define h (hash key))
  (define idx (mask h 0))
  (define node (vector-ref hvector idx))
  (define initial-level (level-up 0))

  (cond ((not node)
         (vector-set hvector idx (make-leaf key (update base))))
        ((leaf? node)
         (vector-set hvector idx (handle-leaf node initial-level)))
        ((collision? node)
         (vector-set hvector idx (handle-collision node initial-level)))
        (else
         (vector-set hvector idx (handle-subtrie node initial-level)))))

(define (delete vector key hash eqv?)
  (define (handle-subtrie subtrie level)
    (define bitmap  (subtrie-bitmap subtrie))
    (define vector  (subtrie-vector subtrie))
    (define index   (mask h level))
    (define (fixup node)
      (update bitmap vector index node))
    (if (not (bitwise-bit-set? bitmap index))
        subtrie
        (let ((node (vector-ref vector (ctpop bitmap index))))
          (cond ((leaf? node)
                 (fixup (handle-leaf node)))
                ((collision? node)
                 (fixup (handle-collision node)))
                (else
                 (fixup (handle-subtrie node (level-up level))))))))

  (define (update bitmap vector index value)
    (if value
        (make-subtrie bitmap (vector-set vector index value))
        (let ((vector* (vector-remove vector index)))
          (if (equal? '#() vector)
              #f
              (make-subtrie (bitwise-bit-unset bitmap index)
                            vector*)))))

  (define (handle-leaf node)
    (if (eqv? key (leaf-key node))
        #f
        node))

  (define (handle-collision node)
    (let ((al (alist-delete (collision-alist node) key eqv?)))
      (cond ((null? (cdr al))
             (make-leaf (car (car al)) (cdr (car al))))
            (else
             (make-collision (collision-hash node) al)))))
  
  (define h (hash key))
  (define idx (mask h 0))
  (define node (vector-ref vector idx))

  (cond ((not node) vector)
        ((leaf? node)
         (vector-set vector idx (handle-leaf node)))
        ((collision? node)
         (vector-set vector idx (handle-collision node)))
        (else
         (vector-set vector idx (handle-subtrie node (level-up 0))))))

(define (vec-map mapper vector)
  (define (handle-subtrie trie)
    (make-subtrie (subtrie-bitmap trie)
                  (vector-map dispatch (subtrie-vector vector))))

  (define (handle-leaf leaf)
    (make-leaf (leaf-key leaf)
               (mapper (leaf-value leaf))))

  (define (handle-collision collision)
    (make-collision (collision-hash collision)
                    (map (lambda (pair)
                           (cons (car pair) (mapper (cdr pair))))
                         (collision-alist collision))))

  (define (dispatch val)
    (cond ((leaf? val)
           (handle-leaf val))
          ((collision? val)
           (handle-collision val))
          (else
           (handle-subtrie val))))

  (vector-map (lambda (val)
                ;; top can have #f values
                (and val (dispatch val)))
              vector))

(define (fold combine initial vector)
  (define (handle-subtrie trie accum)
    (vector-fold dispatch accum (subtrie-vector vector)))

  (define (handle-leaf leaf accum)
    (combine (leaf-key leaf) (leaf-value leaf) accum))

  (define (handle-collision collision accum)
    (fold-right (lambda (pair acc)
                  (combine (car pair) (cdr pair) acc))
                accum
                (collision-alist collision)))

  (define (dispatch val accum)
    (cond ((leaf? val)
           (handle-leaf val accum))
          ((collision? val)
           (handle-collision val accum))
          (else
           (handle-subtrie val accum))))

  (vector-fold (lambda (val accum)
                 ;; top level can have false values
                 (if (not val) accum (dispatch val accum)))
               initial
               vector))

(define (size node)
  (cond ((not node) 0)
        ((leaf? node) 1)
        ((collision? node) (collision-size node))
        (else (subtrie-size node))))

;;; Exported Interface

(define-record-type (hamt %make-hamt hamt?)
  (fields size root hash-function equivalence-predicate))

(define (wrap-root root hamt)
  (define vecsize
    (vector-fold (lambda (val accum)
                   (+ (size val) accum))
                 0
                 root))
  (%make-hamt vecsize
              root
              (hamt-hash-function hamt)
              (hamt-equivalence-predicate hamt)))

(define (make-hamt hash eqv?)
  (%make-hamt 0 (make-vector cardinality #f) hash eqv?))

(define hamt-ref
  (case-lambda
    ((hamt key)
     (define token (cons #f #f))
     (define return-val (hamt-ref hamt key token))
     (when (eqv? token return-val)
       (assertion-violation 'hamt-ref "Key is not in the hamt" key))
     return-val)
    ((hamt key default)
     ;; assert hamt?
     (lookup (hamt-root hamt)
             key
             default
             (hamt-hash-function hamt)
             (hamt-equivalence-predicate hamt)))))

(define (hamt-set hamt key value)
  (define root
    (insert (hamt-root hamt)
            key
            (lambda (old) value)
            'dummy
            (hamt-hash-function hamt)
            (hamt-equivalence-predicate hamt)))
  (wrap-root root hamt))

(define (hamt-update hamt key proc default)
  (define root
    (insert (hamt-root hamt)
            key
            proc
            default
            (hamt-hash-function hamt)
            (hamt-equivalence-predicate hamt)))
  (wrap-root root hamt))

(define (hamt-delete hamt key)
  (define root
    (delete (hamt-root hamt)
            key
            (hamt-hash-function hamt)
            (hamt-equivalence-predicate hamt)))
  (wrap-root root hamt))

(define (hamt-contains? hamt key)
  (define token (cons #f #f))
  (if (eqv? token (hamt-ref hamt key token))
      #f
      #t))

(define (hamt-map mapper hamt)
  (%make-hamt (hamt-size hamt)
              (vec-map mapper (hamt-root hamt))
              (hamt-hash-function hamt)
              (hamt-equivalence-predicate hamt)))

(define (hamt-fold combine initial hamt)
  (fold combine initial (hamt-root hamt)))

(define (hamt->alist hamt)
  (hamt-fold (lambda (key value accumulator)
               (cons (cons key value) accumulator))
             '()
             hamt))

(define (alist->hamt alist hash eqv?)
  (fold-right (lambda (kv-pair hamt)
                (hamt-set hamt (car kv-pair) (cdr kv-pair)))
              (make-hamt hash eqv?)
              alist))

)
