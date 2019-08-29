#!r6rs
(library (pfds tests sets)
(export sets)
(import (rnrs)
        (wak trc-testing)
        (pfds tests utils)
        (pfds sets))

(define-test-suite sets
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

)
