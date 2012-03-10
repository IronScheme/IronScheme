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
        (wak trc-testing))

(define (foldl kons knil list)
  (if (null? list)
      knil
      (foldl kons (kons (car list) knil) (cdr list))))

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

(run-test pfds)
