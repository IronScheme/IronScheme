#!r6rs
;; Copyright (C) 2011,2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Code:
(import (rnrs)
        (pfds queues)
        (pfds deques)
        (pfds bbtrees)
        (pfds sets)
        (pfds psqs)
        (pfds heaps)
        (rename (pfds fingertrees)
                (make-fingertree %make-fingertree)
                (list->fingertree %list->fingertree))
        (pfds sequences)
        (wak trc-testing))

(define (add1 x)
  (+ x 1))

(define (foldl kons knil list)
  (if (null? list)
      knil
      (foldl kons (kons (car list) knil) (cdr list))))

(define (alist->psq alist key<? priority<?)
  (foldl (lambda (kv psq)
           (psq-set psq (car kv) (cdr kv)))
         (make-psq key<? priority<?)
         alist))

;; Right now, I am not testing the monoidal parts of fingertrees, so
;; we use constructor that replaces these with arbitrary values
(define (make-fingertree)
  (%make-fingertree 0 (lambda (x y) x) (lambda (x) x)))

(define (list->fingertree l)
  (%list->fingertree l 0 (lambda (x y) x) (lambda (x) x)))

(define (list->product-tree l)
  (%list->fingertree l 1 * values))

(define (list->last-tree l)
  (define *cookie* (cons 'no 'last))
  (define (pick x y)
    (if (eq? *cookie* y)
        x
        y))
  (%list->fingertree l *cookie* pick values))

(define-syntax test
  (syntax-rules ()
    ((test body)
     (test-eqv #t (and body #t)))))

(define-syntax test-not
  (syntax-rules ()
    ((test-not body)
     (test-eqv #f body))))

(define-syntax test-exn
  (syntax-rules ()
    ((test-exn exception-pred? body)
     (test-eqv #t
               (guard (exn ((exception-pred? exn) #t)
                           (else #f))
                 body
                 #f)))))

(define-syntax test-no-exn
  (syntax-rules ()
    ((test-no-exn body)
     (test-eqv #t
               (guard (exn (else #f))
                 body
                 #t)))))

(define-test-suite pfds
  "Test suite for libraries under the (pfds) namespace")

(define-test-suite (queues pfds)
  "Tests for the functional queue implementation")

(define-test-case queues empty-queue ()
  (test-predicate queue? (make-queue))
  (test-predicate queue-empty? (make-queue))
  (test-eqv 0 (queue-length (make-queue))))

(define-test-case queues enqueue ()
  (let ((queue (enqueue (make-queue) 'foo)))
    (test-predicate queue? queue)
    (test-eqv #t (not (queue-empty? queue)))
    (test-eqv 1 (queue-length queue))
    (test-eqv 10 (queue-length
                  (foldl (lambda (val queue)
                           (enqueue queue val))
                         (make-queue)
                         '(0 1 2 3 4 5 6 7 8 9))))))

(define-test-case queues dequeue ()
  (let ((empty (make-queue))
        (queue1 (enqueue (make-queue) 'foo))
        (queue2 (enqueue (enqueue (make-queue) 'foo) 'bar)))
    (let-values (((item queue) (dequeue queue1)))
      (test-eqv 'foo item)
      (test-predicate queue? queue)
      (test-predicate queue-empty? queue))
    (let*-values (((first queue*) (dequeue queue2))
                  ((second queue) (dequeue queue*)))
                 (test-eqv 'foo first)
                 (test-eqv 'bar second)
                 (test-eqv 1 (queue-length queue*))
                 (test-eqv 0 (queue-length queue)))
    (test-eqv #t
              (guard (exn ((queue-empty-condition? exn) #t)
                          (else #f))
                (dequeue empty)
                #f))))


(define-test-case queues queue-ordering ()
  (let* ((list '(bar quux foo zot baz))
         (queue (list->queue list)))
    (test-eqv 5 (queue-length queue))
    (test-equal list (queue->list queue))))


(define-test-suite (deques pfds)
  "Tests for the functional deque implementation")

(define-test-case deques empty-deque ()
  (test-predicate deque? (make-deque))
  (test-predicate deque-empty? (make-deque))
  (test-eqv 0 (deque-length (make-deque))))

(define-test-case deques deque-insert ()
  (let ((deq (enqueue-front (make-deque) 'foo)))
    (test-predicate deque? deq)
    (test-eqv 1 (deque-length deq)))
  (let ((deq (enqueue-rear (make-deque) 'foo)))
    (test-predicate deque? deq)
    (test-eqv 1 (deque-length deq)))
  (test-eqv 5 (deque-length
               (foldl (lambda (pair deque)
                        ((car pair) deque (cdr pair)))
                      (make-deque)
                      `((,enqueue-front . 0)
                        (,enqueue-rear  . 1)
                        (,enqueue-front . 2)
                        (,enqueue-rear  . 3)
                        (,enqueue-front . 4))))))

(define-test-case deques deque-remove ()
  (let ((deq (enqueue-front (make-deque) 'foo)))
    (let-values (((item0 deque0) (dequeue-front deq))
                 ((item1 deque1) (dequeue-rear deq)))
      (test-eqv 'foo item0)
      (test-eqv 'foo item1)
      (test-predicate deque-empty? deque0)
      (test-predicate deque-empty? deque1)))
  (let ((deq (foldl (lambda (item deque)
                      (enqueue-rear deque item))
                    (make-deque)
                    '(0 1 2 3 4 5))))
    (let*-values (((item0 deque0) (dequeue-front deq))
                  ((item1 deque1) (dequeue-front deque0))
                  ((item2 deque2) (dequeue-front deque1)))
      (test-eqv 0 item0)
      (test-eqv 1 item1)
      (test-eqv 2 item2)
      (test-eqv 3 (deque-length deque2))))
  (let ((deq (foldl (lambda (item deque)
                      (enqueue-rear deque item))
                    (make-deque)
                    '(0 1 2 3 4 5))))
    (let*-values (((item0 deque0) (dequeue-rear deq))
                  ((item1 deque1) (dequeue-rear deque0))
                  ((item2 deque2) (dequeue-rear deque1)))
      (test-eqv 5 item0)
      (test-eqv 4 item1)
      (test-eqv 3 item2)
      (test-eqv 3 (deque-length deque2))))
  (let ((empty (make-deque)))
    (test-eqv #t
              (guard (exn ((deque-empty-condition? exn) #t)
                          (else #f))
                (dequeue-front empty)
                #f))
    (test-eqv #t
              (guard (exn ((deque-empty-condition? exn) #t)
                          (else #f))
                (dequeue-rear empty)
                #f))))


(define-test-case deques mixed-operations ()
  (let ((deque (foldl (lambda (pair deque)
                        ((car pair) deque (cdr pair)))
                      (make-deque)
                      `((,enqueue-front . 0)
                        (,enqueue-rear  . 1)
                        (,enqueue-front . 2)
                        (,enqueue-rear  . 3)
                        (,enqueue-front . 4)))))
    (let*-values (((item0 deque) (dequeue-front deque))
                  ((item1 deque) (dequeue-front deque))
                  ((item2 deque) (dequeue-front deque))
                  ((item3 deque) (dequeue-front deque))
                  ((item4 deque) (dequeue-front deque)))
      (test-eqv 4 item0)
      (test-eqv 2 item1)
      (test-eqv 0 item2)
      (test-eqv 1 item3)
      (test-eqv 3 item4)))
  (let ((deq (foldl (lambda (item deque)
                      (enqueue-rear deque item))
                    (make-deque)
                    '(0 1 2))))
    (let*-values (((item0 deque0) (dequeue-rear deq))
                  ((item1 deque1) (dequeue-front deque0))
                  ((item2 deque2) (dequeue-rear deque1)))
      (test-eqv 2 item0)
      (test-eqv 0 item1)
      (test-eqv 1 item2)
      (test-predicate deque-empty? deque2))))

(define-test-case deques list-conversion ()
  (let ((id-list (lambda (list)
                   (deque->list (list->deque list))))
        (l1 '())
        (l2 '(1 2 3))
        (l3 '(4 5 6 7 8 9 10))
        (l4 (string->list "abcdefghijklmnopqrstuvwxyz")))
    (test-equal l1 (id-list l1))
    (test-equal l2 (id-list l2))
    (test-equal l3 (id-list l3))
    (test-equal l4 (id-list l4))))

(define-test-suite (bbtrees pfds)
  "Tests for the bounded balance tree imlementation")

(define-test-case bbtrees empty-tree ()
  (test-predicate bbtree? (make-bbtree <))
  (test-eqv 0 (bbtree-size (make-bbtree <))))

(define-test-case bbtrees bbtree-set ()
  (let* ([tree1 (bbtree-set (make-bbtree <) 1 'a)]
         [tree2 (bbtree-set tree1 2 'b)]
         [tree3 (bbtree-set tree2 1 'c )])
    (test-eqv 1 (bbtree-size tree1))
    (test-eqv 'a (bbtree-ref tree1 1))
    (test-eqv 2 (bbtree-size tree2))
    (test-eqv 'b (bbtree-ref tree2 2))
    (test-eqv 2 (bbtree-size tree3))
    (test-eqv 'c (bbtree-ref tree3 1))
    (test-eqv #f (bbtree-ref tree1 #xdeadbeef #f))
    (test-eqv 'not-in (bbtree-ref tree1 #xdeadbeef 'not-in))
    (test-exn assertion-violation? (bbtree-ref tree3 20))))

(define-test-case bbtrees bbtree-update ()
  (let ([bb (alist->bbtree '(("foo" . 10) ("bar" . 12)) string<?)]
        [add1 (lambda (x) (+ x 1))])
    (test-case bbtree-update ()
      (test-eqv 11 (bbtree-update bb "foo" add1 0))
      (test-eqv 13 (bbtree-update bb "bar" add1 0))
      (test-eqv  1 (bbtree-update bb "baz" add1 0)))))

(define-test-case bbtrees bbtree-delete ()
  (let* ([tree1 (bbtree-set (bbtree-set (bbtree-set (make-bbtree string<?) "a" 3)
                                        "b"
                                        8)
                            "c"
                            19)]
         [tree2 (bbtree-delete tree1 "b")]
         [tree3 (bbtree-delete tree2 "a")])
    (test-eqv 3 (bbtree-size tree1))
    (test-eqv #t (bbtree-contains? tree1 "b"))
    (test-eqv #t (bbtree-contains? tree1 "a"))
    (test-eqv 2 (bbtree-size tree2))
    (test-eqv #f (bbtree-contains? tree2 "b"))
    (test-eqv #t (bbtree-contains? tree2 "a"))
    (test-eqv 1 (bbtree-size tree3))
    (test-eqv #f (bbtree-contains? tree3 "a"))
    (test-no-exn (bbtree-delete (bbtree-delete tree3 "a") "a"))))

(define-test-case bbtrees bbtree-folds
  (let ((bb (alist->bbtree '(("foo" . 1) ("bar" . 12) ("baz" . 7)) string<?)))
    (test-case bbtree-folds ()
      ;; empty case
      (test-eqv #t (bbtree-fold (lambda args #f) #t (make-bbtree >)))      
      (test-eqv #t (bbtree-fold-right (lambda args #f) #t (make-bbtree >)))
      ;; associative operations
      (test-eqv 20 (bbtree-fold (lambda (key value accum) (+ value accum)) 0 bb))
      (test-eqv 20 (bbtree-fold-right (lambda (key value accum) (+ value accum)) 0 bb))
      ;; non-associative operations
      (test-equal '("foo" "baz" "bar")
                  (bbtree-fold (lambda (key value accum) (cons key accum)) '() bb))
      (test-equal '("bar" "baz" "foo")
                  (bbtree-fold-right (lambda (key value accum) (cons key accum)) '() bb)))))

(define-test-case bbtrees bbtree-map
  (let ((empty (make-bbtree <))
        (bb (alist->bbtree '((#\a . foo) (#\b . bar) (#\c . baz) (#\d . quux))
                           char<?)))
    (test-case bbtree-map ()
      (test-eqv 0 (bbtree-size (bbtree-map (lambda (x) 'foo) empty)))
      (test-equal '((#\a foo . foo) (#\b bar . bar) (#\c baz . baz) (#\d quux . quux))
                  (bbtree->alist (bbtree-map (lambda (x) (cons x x)) bb)))
      (test-equal '((#\a . "foo") (#\b . "bar") (#\c . "baz") (#\d . "quux"))
                  (bbtree->alist (bbtree-map symbol->string bb))))))

(define-test-case bbtrees conversion ()
  (test-eqv '() (bbtree->alist (make-bbtree <)))
  (test-eqv 0 (bbtree-size (alist->bbtree '() <)))
  (test-equal '(("bar" . 12) ("baz" . 7) ("foo" . 1))
              (bbtree->alist (alist->bbtree '(("foo" . 1) ("bar" . 12) ("baz" . 7)) string<?)))
  (let ((l '(48 2 89 23 7 11 78))
        (tree-sort  (lambda (< l)
                      (map car
                           (bbtree->alist
                            (alist->bbtree (map (lambda (x) (cons x 'dummy))
                                                l)
                                           <))))))
    (test-equal (list-sort < l) (tree-sort < l))))

(define-test-case bbtrees bbtree-union
  (let ([empty (make-bbtree char<?)]
        [bbtree1 (alist->bbtree '((#\g . 103) (#\u . 117) (#\i . 105) (#\l . 108) (#\e . 101))
                                char<?)]
        [bbtree2 (alist->bbtree '((#\l . 8) (#\i . 5) (#\s . 15) (#\p . 12))
                                char<?)])
    (test-case bbtree-union ()
      (test-eqv 0 (bbtree-size (bbtree-union empty empty)))
      (test-eqv (bbtree-size bbtree1)
                (bbtree-size (bbtree-union empty bbtree1)))
      (test-eqv (bbtree-size bbtree1)
                (bbtree-size (bbtree-union bbtree1 empty)))
      (test-eqv (bbtree-size bbtree1)
                (bbtree-size (bbtree-union bbtree1 bbtree1)))
      (test-equal '(#\e #\g #\i #\l #\p #\s #\u)
                  (bbtree-keys (bbtree-union bbtree1 bbtree2)))
      ;; union favours values in first argument when key exists in both
      (let ((union (bbtree-union bbtree1 bbtree2)))
        (test-eqv 105 (bbtree-ref union #\i))
        (test-eqv 108 (bbtree-ref union #\l)))
      ;; check this holds on larger bbtrees
      (let* ([l (string->list "abcdefghijlmnopqrstuvwxyz")]
             [b1 (map (lambda (x) (cons x (char->integer x))) l)]
             [b2 (map (lambda (x) (cons x #f)) l)])
        (test-equal b1
                    (bbtree->alist (bbtree-union (alist->bbtree b1 char<?)
                                                 (alist->bbtree b2 char<?))))))))

(define-test-case bbtrees bbtree-intersection
  (let ([empty (make-bbtree char<?)]
        [bbtree1 (alist->bbtree '((#\g . 103) (#\u . 117) (#\i . 105) (#\l . 108) (#\e . 101))
                                char<?)]
        [bbtree2 (alist->bbtree '((#\l . 8) (#\i . 5) (#\s . 15) (#\p . 12))
                                char<?)])
    (test-case bbtree-intersection ()
      (test-eqv 0 (bbtree-size (bbtree-intersection empty empty)))
      (test-eqv 0 (bbtree-size (bbtree-intersection bbtree1 empty)))
      (test-eqv 0 (bbtree-size (bbtree-intersection empty bbtree1)))
      (test-eqv (bbtree-size bbtree1)
                (bbtree-size (bbtree-intersection bbtree1 bbtree1)))
      ;; intersection favours values in first set
      (test-equal '((#\i . 105) (#\l . 108))
                  (bbtree->alist (bbtree-intersection bbtree1 bbtree2)))
      ;; check this holds on larger bbtrees
      (let* ([l (string->list "abcdefghijlmnopqrstuvwxyz")]
             [b1 (map (lambda (x) (cons x (char->integer x))) l)]
             [b2 (map (lambda (x) (cons x #f)) l)])
        (test-equal b1
                    (bbtree->alist (bbtree-intersection (alist->bbtree b1 char<?)
                                                        (alist->bbtree b2 char<?)))))
      ;; definition of intersection is equivalent to two differences
      (test-equal (bbtree->alist (bbtree-intersection bbtree1 bbtree2))
                  (bbtree->alist
                   (bbtree-difference bbtree1
                                      (bbtree-difference bbtree1 bbtree2)))))))

(define-test-case bbtrees bbtree-difference
  (let ([empty (make-bbtree char<?)]
        [bbtree1 (alist->bbtree '((#\g . 103) (#\u . 117) (#\i . 105) (#\l . 108) (#\e . 101))
                                char<?)]
        [bbtree2 (alist->bbtree '((#\l . 8) (#\i . 5) (#\s . 15) (#\p . 12))
                                char<?)])
    (test-case bbtree-difference ()
      (test-eqv 0 (bbtree-size (bbtree-difference empty empty)))
      (test-eqv 5 (bbtree-size (bbtree-difference bbtree1 empty)))
      (test-eqv 0 (bbtree-size (bbtree-difference empty bbtree1)))
      (test-eqv 0 (bbtree-size (bbtree-difference bbtree1 bbtree1)))
      (test-equal '((#\e . 101) (#\g . 103) (#\u . 117))
                  (bbtree->alist (bbtree-difference bbtree1 bbtree2)))
      (test-equal '((#\p . 12) (#\s . 15))
                  (bbtree->alist (bbtree-difference bbtree2 bbtree1))))))

(define-test-case bbtrees bbtree-indexing
  (let* ([l (string->list "abcdefghijklmno")]
         [bb (alist->bbtree (map (lambda (x) (cons x #f)) l) char<?)])
    "tnerfgxukscjmwhaod yz"
    (test-case bbtree-difference ()
      (test-equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
                  (map (lambda (x) (bbtree-index bb x)) l))
      (test-exn assertion-violation? (bbtree-index bb #\z))
      (test-equal l
                  (map (lambda (x)
                         (let-values ([(k v) (bbtree-ref/index bb x)])
                           k))
                       '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)))
      (test-exn assertion-violation? (bbtree-ref/index bb -1))
      (test-exn assertion-violation? (bbtree-ref/index bb 15)))))


(define-test-suite (sets pfds)
  "Tests for the set implementation")

(define-test-case sets set-basics
  (let ([empty (make-set string<?)]
        [set (fold-left set-insert
                        (make-set string<?)
                        (list "foo" "bar" "baz" "quux" "zot"))])
    (test-case set-basics ()
      (test-predicate set? empty)
      (test-predicate set? set)
      (test-eqv 0 (set-size empty))
      (test-eqv 5 (set-size set))
      (test-eqv #f (set-member? empty "foo"))
      (test-eqv #t (set-member? (set-insert empty "foo") "foo"))
      (test-eqv #t (set-member? set "foo"))
      (test-eqv #f (set-member? (set-remove set "foo") "foo"))
      (test-no-exn (set-remove empty "anything"))
      (test-no-exn (set-insert set "quux"))
      (test-eqv (set-size (set-insert empty "foo"))
                (set-size (set-insert (set-insert empty "foo") "foo")))
      (test-eqv (set-size (set-remove set "foo"))
                (set-size (set-remove (set-remove set "foo") "foo"))))))

(define-test-case sets set-equality
  (let* ([empty (make-set string<?)]
         [set1  (list->set '("foo" "bar" "baz") string<?)]
         [set2  (list->set '("foo" "bar" "baz" "quux" "zot") string<?)]
         [sets  (list empty set1 set2)])
    (test-case set-equality ()
      (test (for-all (lambda (x) (set=? x x)) sets))
      (test (for-all (lambda (x) (subset? x x)) sets))
      (test-not (exists (lambda (x) (proper-subset? x x)) sets))
      (test (set<? empty set1))
      (test (set<? set1 set2))
      (test (set=? (set-insert set1 "quux")
                   (set-remove set2 "zot"))))))

(define-test-case sets set-operations
  (let* ([empty (make-set <)]
         [set1 (list->set '(0 2 5 7 12 2 3 62 5) <)]
         [set2 (list->set '(94 33 44 2 73 55 48 92 98 29
                            28 98 55 20 69 5 33 53 89 50)
                          <)]
         [sets (list empty set1 set2)])
    (test-case set-operations ()
      (test (for-all (lambda (x) (set=? x (set-union x x))) sets))
      (test (for-all (lambda (x) (set=? x (set-intersection x x))) sets))
      (test (for-all (lambda (x) (set=? empty (set-difference x x))) sets))
      (test (for-all (lambda (x) (set=? x (set-union empty x))) sets))
      (test (for-all (lambda (x) (set=? empty (set-intersection empty x))) sets))
      (test (for-all (lambda (x) (set=? x (set-difference x empty))) sets))
      (test (for-all (lambda (x) (set=? empty (set-difference empty x))) sets))

      (test (set=? (set-union set1 set2) (set-union set2 set1)))
      (test (set=? (set-union set1 set2)
                   (list->set '(0 2 3 69 7 73 12 20 89 28
                                29 94 5 33 98 92 44 48 50 53
                                55 62)
                              <)))

      (test (set=? (set-intersection set1 set2) (set-intersection set2 set1)))
      (test (set=? (set-intersection set1 set2)
                   (list->set '(2 5) <)))
      (test (set=? (set-difference set1 set2)
                   (list->set '(0 3 12 62 7) <)))
      (test (set=? (set-difference set2 set1)
                   (list->set '(33 98 69 73 44 48 92 50 20 53
                                55 89 28 29 94)
                              <))))))

(define-test-case sets set-conversion ()
  (test-eqv '() (set->list (make-set <)))
  (test-eqv 0 (set-size (list->set '() <)))
  (test-equal (string->list "abcdefghijklmno")
              (list-sort char<?
                         (set->list
                          (list->set (string->list "abcdefghijklmno") char<?))))
  (test-equal '(0) (set->list (fold-left set-insert (make-set <) '(0 0 0 0)))))

(define-test-case sets set-iterators ()
  (test-eqv 0 (set-fold + 0 (list->set '() <)))
  (test-eqv 84 (set-fold + 0 (list->set '(3 12 62 7) <)))
  (test-eqv 499968 (set-fold * 1 (list->set '(3 12 62 7 8 4) <))))


(define-test-suite (psqs pfds)
  "Tests for the functional priority search tree implementation")

(define-test-case psqs empty-psq ()
  (test-predicate psq? (make-psq string<? <))
  (test-predicate psq-empty? (make-psq string<? <))
  (test-predicate zero? (psq-size (make-psq string<? <))))

(define-test-case psqs psq-set
  (let* ((empty (make-psq char<? <))
         (psq1  (psq-set empty #\a 10))
         (psq2  (psq-set psq1 #\b 33))
         (psq3  (psq-set psq2 #\c 3))
         (psq4  (psq-set psq3 #\a 12)))
    (test-case psq-set ()
      (test-eqv 10 (psq-ref psq1 #\a))
      (test-exn assertion-violation? (psq-ref psq1 #\b))
      (test-eqv 1 (psq-size psq1))
      
      (test-eqv 10 (psq-ref psq2 #\a))
      (test-eqv 33 (psq-ref psq2 #\b))
      (test-not (psq-contains? psq2 #\c))
      (test-eqv 2 (psq-size psq2))
      
      (test-eqv 10 (psq-ref psq3 #\a))
      (test-eqv 33 (psq-ref psq3 #\b))
      (test-eqv 3  (psq-ref psq3 #\c))
      (test-eqv 3 (psq-size psq3))

      (test-eqv 12 (psq-ref psq4 #\a))
      (test-eqv 33 (psq-ref psq4 #\b))
      (test-eqv 3  (psq-ref psq4 #\c))
      (test-eqv 3 (psq-size psq4)))))

(define-test-case psqs psq-delete
  (let* ((psq1 (alist->psq '((#\a . 10) (#\b . 33) (#\c . 3))
                           char<?
                           <))
         (psq2 (psq-delete psq1 #\c))
         (psq3 (psq-delete psq2 #\b))
         (psq4 (psq-delete psq3 #\a))
         (psq5 (psq-delete psq1 #\d)))
    (test-case psq-delete ()
      (test-eqv #t (psq-contains? psq1 #\c))
      (test-not (psq-contains? psq2 #\c))
      (test-eqv #t (psq-contains? psq2 #\b))
      (test-not (psq-contains? psq3 #\b))
      (test-eqv #t (psq-contains? psq3 #\a))
      (test-predicate psq-empty? psq4)
      (test-eqv (psq-size psq1)
                (psq-size psq5)))))

(define-test-case psqs psq-update
  (let* ((empty (make-psq char<? <))
         (psq1  (psq-update empty #\a add1 10))
         (psq2  (psq-update psq1 #\b add1 33))
         (psq3  (psq-update psq2 #\c add1 3))
         (psq4  (psq-update psq3 #\a add1 0))
         (psq5  (psq-update psq3 #\c add1 0)))
    (test-case psq-update ()
      (test-eqv 11 (psq-ref psq3 #\a))
      (test-eqv 34 (psq-ref psq3 #\b))
      (test-eqv 4  (psq-ref psq3 #\c))
      
      (test-eqv 12 (psq-ref psq4 #\a))
      (test-eqv 34 (psq-ref psq4 #\b))
      (test-eqv 4  (psq-ref psq4 #\c))
      (test-eqv 3  (psq-size psq4))
      
      (test-eqv 11 (psq-ref psq5 #\a))
      (test-eqv 34 (psq-ref psq5 #\b))
      (test-eqv 5  (psq-ref psq5 #\c))
      (test-eqv 3  (psq-size psq5)))))

(define-test-case psqs priority-queue-functions
  (let* ((psq1 (alist->psq '((#\a . 10) (#\b . 33) (#\c . 3) (#\d . 23) (#\e . 7))
                           char<?
                           <))
         (psq2 (psq-delete-min psq1))
         (psq3 (psq-delete-min (psq-set psq2 #\b 9)))
         (psq4 (make-psq < <)))
    (test-case priority-queue-functions ()
      (test-eqv #\c (psq-min psq1))
      (test-eqv #\e (psq-min psq2))
      (test-exn assertion-violation? (psq-delete-min psq4))
      (test-eqv #\a (psq-min (psq-set psq1 #\a 0)))
      (call-with-values
          (lambda ()
            (psq-pop psq3))
        (lambda (min rest)
          (test-eqv #\b min)
          (test-eqv #\a (psq-min rest)))))))

(define-test-case psqs ranged-functions
  (let* ((alist '((#\f . 24) (#\u . 42) (#\p . 16) (#\s . 34) (#\e . 17)
                  (#\x . 45) (#\l . 14) (#\z . 5) (#\t . 45) (#\r . 41)
                  (#\k . 32) (#\w . 14) (#\d . 12) (#\c . 16) (#\m . 20) (#\j . 25)))
         (alist-sorted (list-sort (lambda (x y)
                                    (char<? (car x) (car y)))
                                  alist))
         (psq  (alist->psq alist char<? <)))
    (test-case ranged-functions ()
      (test-equal alist-sorted
                  (psq-at-most psq +inf.0))
      (test-equal '() (psq-at-most psq 0))
      (test-equal '((#\c . 16) (#\d . 12) (#\e . 17) (#\l . 14)
                    (#\m . 20) (#\p . 16) (#\w . 14) (#\z . 5))
                  (psq-at-most psq 20))
      (test-equal alist-sorted
                  (psq-at-most-range psq +inf.0 #\x00 #\xFF))
      ;; with bounds outwith range in psq, is the same as psq-at-most
      (test-equal '() (psq-at-most-range psq 0 #\x00 #\xFF))
      (test-equal '((#\c . 16) (#\d . 12) (#\e . 17) (#\l . 14)
                    (#\m . 20) (#\p . 16) (#\w . 14) (#\z . 5))
                  (psq-at-most-range psq 20 #\x00 #\xFF))
      (test-equal '((#\c . 16) (#\d . 12) (#\e . 17) (#\l . 14)
                    (#\m . 20) (#\p . 16) (#\w . 14) (#\z . 5))
                  (psq-at-most psq 20))
      (test-equal (filter (lambda (x) (char<=? #\e (car x) #\u)) alist-sorted)
                  (psq-at-most-range psq +inf.0 #\e #\u))
      (test-equal '() (psq-at-most-range psq 0 #\e #\u))
      (test-equal '((#\e . 17) (#\l . 14) (#\m . 20) (#\p . 16))
                  (psq-at-most-range psq 20 #\e #\u))
      ;; inclusiveness check
      (test-equal '((#\t . 45))
                  (psq-at-most-range psq 80 #\t #\t))
      ;; if lower bound is higher than upper, then nothing
      (test-equal '() (psq-at-most-range psq 80 #\t #\r)))))

(define-test-suite (heaps pfds)
  "Tests for the leftist heap implementation")

(define-test-case heaps empty-heap ()
  (test-predicate heap? (make-heap <))
  (test-predicate heap-empty? (make-heap <))
  (test-eqv 0 (heap-size (heap <)))
  )

(define-test-case heaps heap-insertion
  (let ((h1 (heap < 7 1 13 9 5 3 11))
        (h2 (heap < 4 2 8 10 6 0 12)))
    (test-case heap-insertion ()
      (test-equal (+ 1 (heap-size h1))
                  (heap-size (heap-insert h1 0)))
      (test-equal (+ 1 (heap-size h1))
                  (heap-size (heap-insert h1 1)))
      (test-equal '(1 2 3 5 7 9 11 13)
                  (heap->list (heap-insert h1 2)))
      (test-equal '(1 3 4 5 7 9 11 13)
                  (heap->list (heap-insert h1 4)))
      (test-equal '(1 3 5 7 9 11 12 13)
                  (heap->list (heap-insert h1 12)))
      (test-equal '(1 3 5 7 9 11 13 100)
                  (heap->list (heap-insert h1 100)))
      (test-equal '(-2 0 2 4 6 8 10 12)
                  (heap->list (heap-insert h2 -2)))
      (test-equal '(0 0 2 4 6 8 10 12)
                  (heap->list (heap-insert h2 0)))
      (test-equal '(0 2 4 6 8 8 10 12)
                  (heap->list (heap-insert h2 8))))))

(define-test-case heaps heap-deletion
  (let ((h1 (heap < 7 1 13 9 5 3 11))
        (h2 (heap < 4 2 8 6 0)))
    (test-case heap-deletion ()
      (test-equal (- (heap-size h1) 1)
                  (heap-size (heap-delete-min h1)))
      (test-equal 1 (heap-min h1))
      (test-equal 0 (heap-min h2))
      (test-equal 1 (heap-min (heap-delete-min (heap-insert h1 -10))))
      (test-equal 3 (heap-size (heap-delete-min (heap-delete-min h2))))
      (test-equal 4 (heap-min (heap-delete-min (heap-delete-min h2))))
      (test-equal '(7 9 11 13)
                  (heap->list
                   (heap-delete-min (heap-delete-min (heap-delete-min h1)))))
      (test-exn heap-empty-condition? (heap-pop (make-heap <)))
      (test-exn heap-empty-condition? (heap-delete-min (make-heap <)))
      (test-exn heap-empty-condition? (heap-min (make-heap <))))))

(define-test-case heaps sorting
  (let ((l1 '(129 109 146 175 229 48 225 239 129 41
              38 13 187 15 207 70 64 198 79 125))
        (l2 '(72 17 220 158 164 133 20 78 96 230 25
              19 13 17 58 223 37 214 94 195 93 174)))
    (test-case sorting ()
      (test-equal '() (heap-sort < '()))
      (test-equal (list-sort < l1)
                  (heap-sort < l1))
      (test-equal (list-sort < l2)
                  (heap-sort < l2)))))

(define-test-suite (fingertrees pfds)
  "Tests for the fingertree implementation")

(define-test-case fingertrees empty-tree ()
  (test-predicate fingertree? (make-fingertree))
  (test-predicate fingertree-empty? (make-fingertree)))

(define-test-case fingertrees construction
  (let ((l1 '(a b c d e f))
        (l2 '((#t . f) (#t . e) (#t . d) (#t . c) (#t . b) (#t . a)))
        (l3 '((#f . a) (#f . b) (#f . c) (#f . d) (#f . e) (#f . f)))
        (l4 '((#f . b) (#f . c) (#t . a) (#f . d) (#f . e) (#f . f)))
        (l5 '((#f . e) (#t . d) (#t . c) (#t . b) (#f . f) (#t . a)))
        (make (lambda (alist)
                (fold-left (lambda (tree pair)
                             (if (car pair)
                                 (fingertree-cons (cdr pair) tree)
                                 (fingertree-snoc tree (cdr pair))))
                           (make-fingertree)
                           alist)))
        (empty (make-fingertree)))
    (test-case construction ()
      (test-eqv #f (fingertree-empty? (fingertree-cons #f empty)))
      (test-eqv #f (fingertree-empty? (fingertree-snoc empty #f)))
      (test-equal l1 (fingertree->list (make l2)))
      (test-equal l1 (fingertree->list (make l3)))
      (test-equal l1 (fingertree->list (make l4)))
      (test-equal l1 (fingertree->list (make l5))))))

(define-test-case fingertrees removal
  (let* ((l1 '(a b c d e f))
         (f1 (list->fingertree l1))
         (f2 (make-fingertree)))
    (test-case removal ()
      (test-exn fingertree-empty-condition? (fingertree-uncons f2))
      (test-exn fingertree-empty-condition? (fingertree-unsnoc f2))
      (let-values (((head tail) (fingertree-uncons f1)))
        (test-eqv (car l1) head)
        (test-equal (cdr l1) (fingertree->list tail)))
      (let*-values (((init last) (fingertree-unsnoc f1))
                    ((l*) (reverse l1))
                    ((l1-last) (car l*))
                    ((l1-init) (reverse (cdr l*))))
        (test-eqv l1-last last)
        (test-equal l1-init (fingertree->list init))))))

(define-test-case fingertrees conversion
  (let ((l1 '(31 238 100 129 6 169 239 150 96 141 207 208 190 45 56
              183 199 254 78 210 14 131 10 220 205 203 125 111 42 249))
        (l2 '(25 168 21 246 39 211 60 83 103 161 192 201 31 253
              156 218 204 186 155 117)))
    (test-case conversion ()
      (test-equal '() (fingertree->list (list->fingertree '())))
      (test-equal l1 (fingertree->list (list->fingertree l1)))
      (test-equal l2 (fingertree->list (list->fingertree l2))))))

(define-test-case fingertrees ftree-append
  (let ((l1 '(31 238 100 129 6 169 239 150 96 141 207 208 190 45 56
              183 199 254 78 210 14 131 10 220 205 203 125 111 42 249))
        (l2 '(25 168 21 246 39 211 60 83 103 161 192 201 31 253
              156 218 204 186 155 117))
        (append* (lambda (a b)
                   (fingertree->list
                    (fingertree-append
                     (list->fingertree a)
                     (list->fingertree b))))))
    (test-case ftree-append ()
      (test-equal (append l1 '()) (append* l1 '()))
      (test-equal (append '() l1) (append* '() l1))
      (test-equal (append l1 l2) (append* l1 l2))
      (test-equal (append l1 l1) (append* l1 l1))
      (test-equal (append l1 l2) (append* l1 l2)))))

(define-test-case fingertrees monoidal-operation
  (let ((l1 '(31 238 100 129 6 169 239 150 96 141
              207 208 190 45 56 183 199 254 78 210))
        (l2 '((31 238 100 129 6) (169 239 150) (96 141 207 208 190)
              ()  (45 56 183 199) (254 78 210)))
        (car/default (lambda (dflt) (lambda (x) (if (pair? x) (car x) dflt))))
        (list->sum-tree (lambda (l1) (%list->fingertree l1 0 + values))))
    (test-case moniodal-operation ()
      (test-equal 254 (fingertree-measure (%list->fingertree l1 0 max values)))
      (test-equal 6 (fingertree-measure (%list->fingertree l1 1000 min values)))
      (test-equal l1 (fingertree-measure (%list->fingertree l2 '() append values)))
      (test-equal 595 (fingertree-measure
                       (%list->fingertree l2 0 + (car/default 0))))
      ;; sum of l1 is 4239
      (test-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 0))
                                                           (list->sum-tree l1))))
                       (fingertree->list (fingertree-append a b))))
      (test-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 1000))
                                                           (list->sum-tree l1))))
                       (fingertree->list (fingertree-append a b))))
      (test-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 2000))
                                                           (list->sum-tree l1))))
                       (fingertree->list (fingertree-append a b))))
      (test-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 5000))
                                                           (list->sum-tree l1))))
                       (fingertree->list (fingertree-append a b)))))))

(define-test-case fingertrees fingertree-folds
  (let* ((l '(31 238 100 129 6 169 239 150 96 141
                 207 208 190 45 56 183 199 254 78 210))
         (lrev (reverse l))
         (total (apply + l))
         (ft (list->fingertree l)))
    (test-case fingertree-folds ()
      ;; empty case
      (test-eqv #t (fingertree-fold (lambda _ #f) #t (make-fingertree)))
      (test-eqv #t (fingertree-fold-right (lambda _ #f) #t (make-fingertree)))
      ;; associative operations
      (test-eqv total (fingertree-fold + 0 ft))
      (test-eqv total (fingertree-fold-right + 0 ft))
      ;; non-associative operations
      (test-equal lrev (fingertree-fold cons '() ft))
      (test-equal l (fingertree-fold-right cons '() ft)))))

(define-test-case fingertrees reversal
  (let ((rev (lambda (l)
               (fingertree->list
                (fingertree-reverse (list->fingertree l)))))
        (id (lambda (l)
              (fingertree->list
               (fingertree-reverse
                (fingertree-reverse (list->fingertree l))))))
        (l1 '(126 6 48 86 2 119 233 92 230 160))
        (l2 '(25 168 21 246 39 211 60 83 103 161
              192 201 31 253 156 218 204 186 155 117)))
    (test-case reversal ()
      ;; behaves the same as regular reverse on lists
      (test-eqv '() (rev '()))
      (test-equal '(1) (rev '(1)))
      (test-equal '(6 5 4 3 2 1) (rev '(1 2 3 4 5 6)))
      (test-equal (reverse l1) (rev l1))
      (test-equal (reverse l2) (rev l2))
      ;; double reversal is the the same list
      (test-equal l1 (id l1))
      (test-equal l2 (id l2))
      ;; a fingertree will have the same measure as its reverse if
      ;; the monoid is commutative
      (test-equal (fingertree-measure (list->product-tree l1))
                  (fingertree-measure
                   (fingertree-reverse (list->product-tree l1))))
      ;; otherwise they are not necessarily the same
      ;; in this case, they are the same only if the first and last
      ;; elements are the same
      (test-not
       (equal? (fingertree-measure (list->last-tree l2))
               (fingertree-measure (fingertree-reverse (list->product-tree l2))))))))


(define-test-suite (sequences pfds)
  "Tests for the sequences implementation")
;; Note: at the moment, sequences are a trivial instantiation of
;; fingertrees, and so are pretty much covered by the fingertrees
;; tests.

(define-test-case sequences sequences-bugs
  (let ((s (sequence 'zero 'one 'two)))
    (test-case sequences-bugs ()
      (test-eqv 'zero (sequence-ref s 0))
      (test-eqv 'two (sequence-ref s 2))
      (test-exn assertion-violation? (sequence-ref s -1))
      (test-exn assertion-violation? (sequence-ref s 3)))))

(run-test pfds)
