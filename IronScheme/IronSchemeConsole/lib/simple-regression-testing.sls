#!r6rs
;;; FILE: "simple-regression-testing.sls"
;;; IMPLEMENTS: Simple [minded] Regression Test System
;;; LANGUAGE: Ikarus Scheme
;;; AUTHOR: Ken Dickey

;;;COPYRIGHT (c) 2005, 2008 by Kenneth A Dickey. All rights reserved.
;;;
;;;Permission is hereby granted, free of charge, to any person
;;;obtaining a copy of this software and associated documentation
;;;files (the "Software"), to deal in the Software without
;;;restriction, including without limitation the rights to use,
;;;copy, modify, merge, publish, distribute, sublicense, and/or
;;;sell copies of the Software, and to permit persons to whom
;;;the Software is furnished to do so, subject to the following
;;;conditions:
;;;
;;;The above copyright notice and this permission notice shall
;;;be included in all copies or substantial portions of the Software.
;;;
;;;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;OTHER DEALINGS IN THE SOFTWARE.
;==============================================================;
(library (simple-regression-testing)
  (export verbose-test-output? break-on-test-error?
          run-all-tests run-tests-for-suite
          add-test-suite remove-test-suite remove-all-test-suites
          add-test add-eq-test add-equal-test ensure-exception-raised
          default-setup-thunk default-teardown-thunk
          )
  (import (rnrs)
          (srfi parameters)
          (srfi format)
          )
  
;==============================================================;
;;
;;;USAGE SYNOPSIS
;;
;; (import (simple-regression-testing))
;; <define one or more test suites & tests>
;; (run-all-tests)
;;
;; Tests are first created and added to a global TESTS "database".
;; Tests are arranged by SUITE-NAME (just a symbol naming a set of tests).
;;
;; SPECIAL FORMS:
;;
;; (add-test       suite-name expect form equivalent? . message)
;; (add-eq-test    suite-name expect form . message)
;; (add-equal-test suite-name expect form . message)
;; (ensure-exception-raised suite-name type-pred? form . message)
;; 
;;  All forms are "thunkified" by being wrapped in zero argument lambdas.
;;  Internal usage is: (equivalent? expected (thunk))
;;
;;
;; TESTING OPERATIONS:
;;
;;  (add-test-suite suite-name setup-thunk teardown-thunk)
;;   => Creates a test suite for the suite-name, which must be a symbol.
;; 
;;  (run-all-tests) => Run all suites of tests.
;;
;;  (run-tests-for-suite suite-name) => Run tests for the named test-suite.
;;  (remove-suite  suite-name) => Remove the named test-suite.
;;
;;  (verbose-test-output?) => If #t, displayes tests that pass as well as fail or cause exceptions
;;  (verbose-test-output? <bool>) => sets verbose-test-output?
;;
;;  (break-on-test-error?) => if #t, will call ERROR when a test fails,
;;                          otherwise tests report & continue.
;;  (break-on-test-error? <bool>) => sets break-on-test-error?
;;
;;
;; Tests are typically written as separate files containing set-up & tear-down code.
;;
;; Nota Bene:  Currently all output goes to (current-output-port).
;;
;;==============================================================;


(define verbose-test-output? (make-parameter #t))
(define break-on-test-error? (make-parameter #t))

(define-syntax add-test
  (syntax-rules ()
    ((_ <suite-name> <expect> <form> <equivalent?>)
     (add-test <suite-name> <expect> <form> <equivalent?> '<form>)
     )
    ((_ <suite-name> <expect> <form> <equivalent?> <message>)
     (test-db-add-test <suite-name>
                       (make-test <expect>
                                  (lambda () <form>)
                                  <equivalent?>
                                  <message>))
     )
) )

(define-syntax add-eq-test
  (syntax-rules ()
    ((_ <suite-name> <expect> <form>)
     (add-test <suite-name> <expect> <form> eq? '<form>)
     )
    ((_ <suite-name> <expect> <form> <message>)
     (add-test <suite-name> <expect> <form> eq? <message>)
     )
) )

(define-syntax add-equal-test
  (syntax-rules ()
    ((_ <suite-name> <expect> <form>)
     (add-test <suite-name> <expect> <form> equal? '<form>)
     )
    ((_ <suite-name> <expect> <form> <message>)
     (add-test <suite-name> <expect> <form> equal? <message>)
     )
) )

(define-syntax ensure-exception-raised
  (syntax-rules ()
    ((_ <suite-name> <condition-pred?> <form>)
     (ensure-exception-raised <suite-name> <condition-pred?> <form> '<form>)
     )
    ((_ <suite-name> <condition-pred?> <form> <message>)
     (test-db-add-test <suite-name>
                       (make-test 'expect-some-kind-of-exception
                                  (lambda ()
                                    (call-with-current-continuation
                                     (lambda (return)
                                       (with-exception-handler
                                        (lambda (exn) (return exn)) ; capture & return
                                        (lambda () <form>)))))
                                  (lambda (actual ignored)
                                    (<condition-pred?> actual))
                                  <message>))
                                    
     )
) )

;==============================================================;
;; minor helpers

(define (simple-error message-string)
  (newline)
  (display "ERROR: ")
  (display message-string)
  (newline)
  (error 'simple-regression-testing
         message-string)
)

(define (warn message-string)
  (newline)
  (display "WARNING: ")
  (display message-string)
)

(define (hashtable-foreach proc table)
  (unless (and (procedure? proc) (hashtable? table))
    (error 'hashtable-foreach
           "Requires a procedure and a hashtable"
           proc table))
  (let-values
      ( ((keyvec valvec) (hashtable-entries table)) )
    (let ( (max-index (- (vector-length valvec) 1)) )
      (when (> max-index -1) ; work to do..
        (let loop ( (index 0) )
          ;; (proc key val)
          (proc (vector-ref keyvec index)
                (vector-ref valvec index))
          (when (< index max-index)
            (loop (+ 1 index))))
) ) ) )

;==============================================================;

(define-record-type test
  (fields (immutable expected)
          (immutable thunk)
          (immutable compare?)
          (immutable message))
) 


;;;A TEST-COUNTER keeps track of number passed,
;;;    failed (actual != expected), excepted (signalled exception)
;;;    and reports on these statistics.

(define-record-type test-counter
  (fields (immutable name)
          (mutable num-passed)
          (mutable num-failed)
          (mutable num-excepted))
  (protocol
   (lambda (constr)
     (lambda (name) (constr name 0 0 0))))
)


(define (counter-increment-failed counter)
  (test-counter-num-failed-set!
   counter
   (+ 1 (test-counter-num-failed counter))))

(define (counter-increment-excepted counter)
  (test-counter-num-excepted-set!
   counter
   (+ 1 (test-counter-num-excepted counter))))

(define (counter-increment-passed counter)
  (test-counter-num-passed-set!
   counter
   (+ 1 (test-counter-num-passed counter))))

(define (counter-display-results counter)
  (display (format "~%Test Results for ~a"      (test-counter-name counter)))
  (display (format "~%TOTAL PASSED:     ~d"     (test-counter-num-passed   counter)))
  (display (format "~%TOTAL FAILED:     ~d"     (test-counter-num-failed   counter)))
  (display (format "~%TOTAL EXCEPTIONS: ~d~%~%" (test-counter-num-excepted counter)))
)

;;;======================================================================
;;;A TEST-SUITE is a container of unit tests, setup and 
;;;   teardown code.  Tests are a reversed list of test
;;;   instances (see below).
;;;
;;;   A test-container maintains the (name -> unit-test-suite) bindings.

(define-record-type test-suite
  (fields (immutable name)
          (mutable test-list)
          (mutable setup-thunk)
          (mutable teardown-thunk))
)

(define (test-suite-add-test suite test)
  (unless (and (test-suite? suite) (test? test))
    (error 'test-suite-add-test
           "Requires a test-suite and a test"
           suite test))
  (test-suite-test-list-set! suite
                             (cons test (test-suite-test-list suite)))
)

(define (test-suite-remove-tests suite)
  (test-suite-test-list-set! suite '()))

(define (test-suite-run-tests suite result-counter verbose? break-on-error?)
  (unless (test-suite? suite)
    (error 'test-suite-run-tests
           "Requires a test-suite"
           suite))
  (let ( (suite-name     (test-suite-name           suite))
         (test-list      (test-suite-test-list      suite))
         (setup-thunk    (test-suite-setup-thunk    suite))
         (teardown-thunk (test-suite-teardown-thunk suite))
       )
   (if (null? test-list)
       ((if break-on-error? simple-error warn)
        (format "HUH?  No tests found for ~a" suite-name))
       (begin
        (setup-thunk)
        (display (format "~%===> Starting  Tests for: ~a" suite-name))
        (for-each 
         (lambda (test)
           (run-test test result-counter verbose? break-on-error?))
         (reverse test-list))
        (display (format "~%===> Completed Tests for: ~a~%" suite-name))
        (teardown-thunk)))
)  )


;;;======================================================================
;;; A TEST is a single test

(define default-setup-thunk
  (lambda () 'no-setup))

(define default-teardown-thunk
  (lambda () 'no-teardown))

;;;======================================================================
;;; A test-container contains and runs named test suites,
;;;   mapping test-suite names to their associated suites.

(define test-container (make-eq-hashtable))

(define (test-container-add! container name suite)
  (unless (and (test-container? container)
               (test-suite? suite)
               (symbol? name))
    (error 'test-container-add!
           "expected: test-container name suite"
           container name suite))
  (hashtable-set! container name suite))


(define (test-container-make-suite container
                                   test-suite-name
                                   setup
                                   teardown)
  (let ( (test-suite
          (make-test-suite test-suite-name '() setup teardown))
       )
    (test-container-add! container test-suite-name test-suite)
    test-suite
) )

(define test-container? hashtable?)

(define (test-container-add-test container suite-name test-case)
  (unless (and (test-container? container)
               (symbol? suite-name)
               (test?   test-case))
    (error 'test-container-add-test
           "expected: container suite-name test"
           test-container-add-test
           container suite-name test-case))
  (cond ((hashtable-ref container suite-name #f)
         => (lambda (suite) (test-suite-add-test suite test-case)))
        (else
         (let ( (suite (test-container-make-suite
                        container
                        suite-name
                        default-setup-thunk
                        default-teardown-thunk))
              )
           (test-suite-add-test suite test-case)
           (warn (format "Created test suite named: ~a" suite-name)))))
  test-case
)

(define (test-container-remove-tests-for container suite-name)
  (unless (and (test-container? container)
               (symbol? suite-name))
    (error 'test-container-remove-tests-for
           "expected: container suite-name"
           container suite-name))
  (hashtable-delete! container suite-name) ;; table-remove!
)

;=================================

;;;======================================================================
;;; RUNNING TESTS

;; Run a test
;; If no error, don't report unless VERBOSE?
;; If error or exception, break into debugger if BREAK-ON-ERROR?, else continue
;; Result-counter is a test-counter

(define (run-test test result-counter verbose? break-on-error?)
   (let* ( (caught-exception #f) 
           (actual 
            (call-with-current-continuation
               (lambda (return)
                 (with-exception-handler
                  (lambda (exn)
                    (set! caught-exception exn)
                    (counter-increment-excepted result-counter)
                    ((if break-on-error? simple-error warn) 
                     (format "--> ~s:~%*** EXCEPTION: ~s, expected: ~s"
                             (test-message test)
                             (if (message-condition? exn)
                                 (condition-message exn)
                                 exn)
                             (test-expected test)))
                    (return exn))
                  (test-thunk test)))))
         )
     (cond
      (caught-exception ;; already processed, above
       #f
       )
      (((test-compare? test) actual (test-expected test))
       (counter-increment-passed result-counter)
       (when verbose?
         (display
          (format "~%--> ~s:~%PASSED: Expected: ~s ~%             Got: ~s"
                  (test-message  test)
                  (test-expected test)
                  (if (message-condition? actual) ;; more informative
                      (condition-message actual)
                      actual))))
       #t) ;; compare => #t
      (else
       (counter-increment-failed result-counter)
       ((if break-on-error? simple-error warn)
        (format "--> ~s:~%*** FAILED:  Expected ~s  Got ~s"
                (test-message test)
                (test-expected test)
                actual))
        #f)
        )
) )


;==============================================================;
;;;EXPORTED BINDINGS


(define add-test-suite
  (lambda (suite-name setup-thunk teardown-thunk)
    (test-container-make-suite
                        test-container
                        suite-name
                        setup-thunk
                        teardown-thunk)
) )

(define remove-test-suite
  (lambda (suite-name)
    (test-container-remove-tests-for test-container suite-name)))

(define (remove-all-test-suites) (set! test-container (make-eq-hashtable)))

(define run-tests-for-suite
  (lambda (suite-name)
    (let ( (suite (hashtable-ref test-container suite-name #f))
           (result-counter (make-test-counter suite-name))
         )
      (if suite
          (begin
            (test-suite-run-tests suite
                                  result-counter
                                  (verbose-test-output?)
                                  (break-on-test-error?))
            (counter-display-results result-counter))
          ((if (break-on-test-error?) simple-error warn)
           (format "HUH?  No tests found for: ~a" suite-name))))
) )

(define run-all-tests
  (lambda ()
    (let ( (result-counter (make-test-counter 'testing)) )
      (hashtable-foreach
       (lambda (suite-name test-suite)
         (if test-suite
             (test-suite-run-tests test-suite
                                   result-counter
                                   (verbose-test-output?)
                                   (break-on-test-error?))
             ((if (break-on-test-error?) simple-error warn)
              (format "HUH?  No tests found for ~a" suite-name))))
        test-container)
      (counter-display-results result-counter)
) ) )


(define test-db-add-test
  (lambda (suite-name test-case)
    (test-container-add-test test-container suite-name test-case)))


)

#!eof
;===========================E=O=F==============================;
