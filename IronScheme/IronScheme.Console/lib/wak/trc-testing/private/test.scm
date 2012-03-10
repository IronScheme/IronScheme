;;; -*- Mode: Scheme -*-

;;;; Testing Utility for Scheme

;;; Copyright (C) 2007, 2009 Taylor R. Campbell.
;;;
;;; This file is part of TRC-Testing.
;;;
;;; TRC-Testing is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; TRC-Testing is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with TRC-Testing.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Parameters:
;;;
;;; (WITH-TEST-CASE-RUN <name> <description> <thunk>)
;;; (WITH-TEST-SUITE-RUN <name> <description> <thunk>)
;;; (NILADIC-TEST)
;;; (MONADIC-TEST <thunk>)
;;; (POLYADIC-TEST <thunks>)
;;; (COMPONENT-TEST <thunk>)
;;; (TEST-FAILURE <message> <irritant> ...)
;;; (TEST-FAILURE:PREDICATE-DATUM <predicate> <expression> <datum>)
;;; (TEST-FAILURE:COMPARE-DATUM <comparator>
;;;                             <expected-expression> <expected-datum>
;;;                             <actual-expression> <actual-datum>)

(define-record-type <test-suite>
    (%make-test-suite name description tests)
    test-suite?
  (name test-suite/name)
  (description test-suite/description)
  (tests test-suite/tests set-test-suite/tests!))

(define (make-test-suite name description)
  (%make-test-suite name description '()))

(define-record-type <test-case>
    (make-test-case name description constructor)
    test-case?
  (name test-case/name)
  (description test-case/description)
  (constructor test-case/constructor))

(define (add-test! suite name test)
  (let ((tests (test-suite/tests suite)))
    (cond ((assv name tests)
           => (lambda (probe)
                (set-cdr! probe test)))
          (else
           (set-test-suite/tests! suite (cons (cons name test) tests))))))

(define (run-test-case test-case)
  (with-test-case-run (test-case/name test-case)
      (test-case/description test-case)
    (lambda ()
      (receive (setup teardown bodies) ((test-case/constructor test-case))
        (define (body->thunk body)
          (lambda ()
            (dynamic-wind setup body teardown)))
        (cond ((not (pair? bodies))
               (niladic-test))
              ((not (pair? (cdr bodies)))
               (monadic-test (body->thunk (car bodies))))
              (else
               (polyadic-test (map body->thunk bodies))))))))

(define (run-test-suite test-suite)
  (with-test-suite-run (test-suite/name test-suite)
      (test-suite/description test-suite)
    (lambda ()
      (for-each (lambda (name.test)
                  (component-test (lambda () (run-test (cdr name.test)))))
                (reverse (test-suite/tests test-suite))))))

(define (run-test test)
  (cond ((test-case? test) (run-test-case test))
        ((test-suite? test) (run-test-suite test))
        (else (error "Invalid test:" test))))

(define (find-test suite name)
  (let loop ((tests (test-suite/tests suite)))
    (cond ((not (pair? tests))
           (error "No such test by name in suite:" name suite))
          ((eqv? name (caar tests))
           (cdar tests))
          (else
           (loop (cdr tests))))))

;;;; Test Macros

(define-syntax test-predicate
  (syntax-rules ()
    ((TEST-PREDICATE predicate expression)
     (LET ((DATUM expression))
       (IF (NOT (predicate expression))
           (TEST-FAILURE:PREDICATE-DATUM 'predicate 'expression DATUM))))))

(define-syntax test-compare
  (syntax-rules ()
    ((TEST-COMPARE comparator expected-expression actual-expression)
     (LET ((EXPECTED-DATUM expected-expression)
           (ACTUAL-DATUM actual-expression))
       (IF (NOT (comparator EXPECTED-DATUM ACTUAL-DATUM))
           (TEST-FAILURE:COMPARE-DATUM 'comparator
                                       'expected-expression EXPECTED-DATUM
                                       'actual-expression ACTUAL-DATUM))))))

(define-syntax test-eq
  (syntax-rules ()
    ((TEST-EQ expected-expression actual-expression)
     (TEST-COMPARE EQ? expected-expression actual-expression))))

(define-syntax test-eqv
  (syntax-rules ()
    ((TEST-EQ expected-expression actual-expression)
     (TEST-COMPARE EQV? expected-expression actual-expression))))

(define-syntax test-equal
  (syntax-rules ()
    ((TEST-EQ expected-expression actual-expression)
     (TEST-COMPARE EQUAL? expected-expression actual-expression))))

;;;; Syntactic Sugar

(define-syntax define-test-suite
  (syntax-rules ()
    ((DEFINE-TEST-SUITE (suite-name parent) description)
     (DEFINE suite-name
       (LET ((suite-name (MAKE-TEST-SUITE 'suite-name 'description)))
         (ADD-TEST! parent 'suite-name suite-name)
         suite-name)))
    ((DEFINE-TEST-SUITE suite-name description)
     (DEFINE suite-name (MAKE-TEST-SUITE 'suite-name 'description)))))

(define-syntax define-test-case
  (syntax-rules ()
    ((DEFINE-TEST-CASE test-suite name test-case)
     (DEFINE-VALUES () ;Make this expand into a definition
       (ADD-TEST! test-suite `name test-case)))
    ((DEFINE-TEST-CASE test-suite test-case-name (option ...) test ...)
     (DEFINE-VALUES () ;Ditto
       (LET ((NAME `test-case-name))
         (ADD-TEST! test-suite NAME
           (TEST-CASE ,NAME (option ...) test ...)))))))

(define-syntax test-case
  (syntax-rules ()
    ;; Do the syntactically fast case with no options.
    ;; WITH-EXTENDED-PARAMETER-OPERATORS* is *slow*.
    ((TEST-CASE test-case-name () test ...)
     (%TEST-CASE test-case-name #F (test ...) ((VALUES)) ((VALUES))))
    ((TEST-CASE test-case-name (option ...) test ...)
     (WITH-EXTENDED-PARAMETER-OPERATORS*
         ((%TEST-CASE*
           ()                      ;No named parameter pattern literals
           (%TEST-CASE
            (NAME               ((NAME ?name)) ?name #F)
            (DESCRIPTION        ((DESCRIPTION ?description)) ?description #F)
            (TESTS              ((TESTS . ?tests)) ?tests #F)
            ;; Unfortunately, because of...issues with ellipsis, we
            ;; can't write the actual patterns we want to write here
            ;; for non-empty proper list bodies.
            (SETUP              ((SETUP . ?setup-body)) ?setup-body ((VALUES)))
            (TEARDOWN           ((TEARDOWN . ?teardown-body))
                                ?teardown-body
                                ((VALUES))))))
       ;; Force named parameters by using leading ones.
       (%TEST-CASE* (NAME test-case-name) (TESTS test ...) option ...)))))

(define-syntax %test-case
  (syntax-rules ()
    ((%TEST-CASE name
                 description
                 (test ...)
                 (setup-body0 setup-body1 ...)
                 (teardown-body0 teardown-body1 ...))
     (MAKE-TEST-CASE `name
                     'description
                     (LAMBDA ()
                       (VALUES (LAMBDA () setup-body0 setup-body1 ...)
                               (LAMBDA () teardown-body0 teardown-body1 ...)
                               (LIST (LAMBDA () test)
                                     ...)))))))
