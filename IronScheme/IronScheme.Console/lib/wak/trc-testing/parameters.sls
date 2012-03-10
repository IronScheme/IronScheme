;;;; Testing Utility for Scheme
;;;; R6RS Parameters

;;; Adaption for R6RS by Andreas Rottmann, originally written by
;;; Taylor Campbell.

;;; Copyright (c) 2009, 2010, Andreas Rottmann
;;; Copyright (c) 2007, Taylor R. Campbell

;;; All code under the GNU Lesser General Public License (see
;;; COPYING.LESSER in the top-level directory).

#!r6rs

(library (wak trc-testing parameters)
  (export
    with-test-case-run
    with-test-suite-run
    niladic-test
    monadic-test
    polyadic-test
    component-test
    test-failure
    test-failure:predicate-datum
    test-failure:compare-datum

    ;; Customization
    test-verbosity      with-test-verbosity     set-test-verbosity!
    test-debug-errors?  with-test-debug-errors? set-test-debug-errors?!
    )
  (import (rnrs)
          (srfi :39 parameters)
          (wak trc-testing limited-write)
          (wak trc-testing display-condition)
          (wak trc-testing port-tracker)
          (wak trc-testing restart))

  (define test-verbosity (make-parameter 'normal))
  (define test-debug-errors? (make-parameter #f))

  (define (with-test-verbosity verbosity thunk)
    (parameterize ((test-verbosity verbosity))
      (thunk)))
  
  (define (set-test-verbosity! verbosity)
    (test-verbosity verbosity))

  (define (with-test-debug-errors? flag thunk)
    (parameterize ((test-debug-errors? flag))
      (thunk)))

  (define (set-test-debug-errors?! flag)
    (test-debug-errors? flag))

;;; Dynamic State

  (define test-name (make-parameter #f))
  (define test-description (make-parameter #f))

  (define testing-suite? (make-parameter #f))

  (define $test-suite-passages (make-parameter #f))
  (define $test-suite-failures (make-parameter #f))

  (define (record-test-passages count)
    (let ((cell ($test-suite-passages)))
      (if (not (cell? cell))
        (error 'trc-testing "Not currenty testing suite."))
      (cell-set! cell (+ (cell-ref cell) count))))

  (define (record-test-failures failures)
    (let ((cell ($test-suite-failures)))
      (if (not (cell? cell))
        (error 'trc-testing "Not currenty testing suite."))
      (cell-set! cell (append-reverse failures (cell-ref cell)))))

  (define (append-reverse list tail)
    (if (pair? list)
        (append-reverse (cdr list) (cons (car list) tail))
        tail))
  
;;;; Test Parameter Implementations

  (define (with-test-case-run name description thunk)
    (with-test-notification name description thunk))

  (define (with-test-suite-run name description thunk)
    (with-test-notification name description
      (lambda ()
        (let ((passages-cell (make-cell 0))
              (failures-cell (make-cell '())))
          (parameterize ((testing-suite? #t)
                         ($test-suite-passages passages-cell)
                         ($test-suite-failures failures-cell))
            (thunk))
          (let ((passages (cell-ref passages-cell))
                (failures (cell-ref failures-cell)))
            (if (testing-suite?)
                (begin (record-test-passages passages)
                       (record-test-failures failures)
                       (if (not (eq? (test-verbosity) 'quiet))
                           (report-test-suite passages failures)))
                (report-test-suite passages failures)))))))

  ;; Move to (spells error)?
  (define (warn message . irritants)
    (raise-continuable (condition (make-warning)
                                  (make-message-condition message)
                                  (make-irritants-condition irritants))))
  
  (define (niladic-test)
    (warn "Null test:" (test-name)))

  (define (monadic-test thunk)
    (with-test-restarters #f
      (lambda ()
        (with-test-condition-handler #f thunk))))

  (define (polyadic-test thunks)
    (with-test-restarters #f
      (lambda ()
        (do ((thunks thunks (cdr thunks))
             (index 0 (+ index 1)))
            ((not (pair? thunks)))
          (with-test-notification index #f
            (lambda ()
              (with-test-restarters index
                (lambda ()
                  (with-test-condition-handler index (car thunks))))))))))

  (define (component-test thunk)
    (thunk))
  
;;;; Test Reporting

  (define (write-string s port)
    (put-string port s))

  
  (define (report-test-suite passages failures)
    (write-notification-line
     (lambda (port)
       (let* ((failures (length failures))
              (total (+ passages failures)))
         (write total port)
         (write-string " tests, " port)
         (write passages port)
         (write-string " passed (" port)
         (write (round (* 100 (/ passages total))) port)
         (write-string "%), " port)
         (write failures port)
         (write-string " failed (" port)
         (write (round (* 100 (/ failures total))) port)
         (write-string "%)" port)
         (newline port)))))

  (define (with-test-notification name description thunk)
    (parameterize ((test-name name)
                   (test-description description))
      (if (eq? (test-verbosity) 'quiet)
          (thunk)
          (with-notification report-test thunk))))

  (define (report-test port)
    (write-string "Test " port)
    (write (test-name) port)
    (if (and (test-description) (eq? (test-verbosity) 'verbose))
        (begin
          (write-string ":" port)
          (write-notification-line
           (lambda (port)
             (write-string (test-description) port))))))

  (define (report-test-failure index condition)
    index                                 ;ignore
    ;; This seems slightly strange, but the reason is that we want to be
    ;; sure to report what tests failed, and if the verbosity setting was
    ;; quiet, then it wouldn't have been reported before the failure.
    (if (eq? (test-verbosity) 'quiet)
        (write-notification-line report-test))
    ;;++ DISPLAY-CONDITION doesn't play nicely with the notification
    ;;++ output utility.  Blah.
    ;;
    ;;++ [It could do so in spells, need to hack it accordingly --rotty]
    (display-condition condition (notification-output-port)))

  (define (test-restarter-description index verb)
    (string-append verb
                   (if index
                       (string-append " test #"
                                      (number->string index)
                                      " of ")
                       " testing ")
                   (call-with-string-output-port
                     (lambda (output-port)
                       (limited-write (test-name) output-port 5 3)))
                   "."))
  
;;;; Test Conditions

  (define (with-test-restarters index thunk)
    (call-with-exiting-restarter 'abort
        (test-restarter-description index "Abort")
      (lambda (exit)
        (let loop ()
          (with-exiting-restarter 'retry
              (test-restarter-description index "Retry")
            (lambda ()
              (thunk)
              (restart exit)))
          (loop)))))

  ;; Try to approximate s48's with-handler on top of R6RS. This will
  ;; only work with raise-continuable
  (define (with-handler handler thunk)
    (with-exception-handler (lambda (e)
                              (handler e (lambda ()
                                           (raise e))))
      thunk))
  
  (define (with-test-condition-handler index thunk)
    (with-handler (lambda (condition propagate)
                    (if (testing-suite?)
                        (record-test-failures (list condition)))
                    (handle-test-failure index condition propagate))
      (lambda ()
        (thunk)
        (if (testing-suite?)
            (record-test-passages 1)))))

  (define (handle-test-failure index condition propagate)
    (if (or (test-failure? condition)
            (and (serious-condition? condition)
                 (not (test-debug-errors?))))
        (begin
          (report-test-failure index condition)
          (restart 'abort))
        (propagate)))

  (define-condition-type &test-failure &error
    make-test-failure test-failure?)

  (define (test-failure message . irritants)
    (raise-continuable (condition
                        (make-test-failure)
                        (make-message-condition message)
                        (make-irritants-condition irritants))))
  
  (define (test-failure:predicate-datum predicate expression datum)
    (test-failure "expression's value failed to satisfy predicate"
                  predicate
                  expression
                  datum))

  (define (test-failure:compare-datum comparator
                                      expected-expression expected-datum
                                      actual-expression actual-datum)
    (test-failure "expression's value failed comparison"
                  `(expected ,expected-expression => ,expected-datum)
                  `(got ,actual-expression => ,actual-datum)))
  
;;;; Nested Notification Utility

  (define force-output port-tracker-flush)

  (define notification-output-port-tracker
    (make-parameter (make-port-tracker (current-output-port))))
  
  (define (notification-output-port)
    (port-tracker-port (notification-output-port-tracker)))

  (define notification-level (make-parameter 0))

  (define (start-notification-line tracker)
    (let ((output-port (port-tracker-port tracker)))
      (port-tracker-fresh-line tracker)
      (write-char #\; output-port)
      (write-string (make-string (* 2 (notification-level))
                                 #\space)
                    output-port)))

  (define (write-notification-line message-writer)
    (let ((tracker (notification-output-port-tracker))
          (output-port (notification-output-port)))
      (start-notification-line tracker)
      (message-writer output-port)
      (force-output tracker)))

  (define (with-notification message-writer thunk)
    (let ((output-port (notification-output-port))
          (tracker (notification-output-port-tracker)))
      (define (start-notification suffix)
        (start-notification-line tracker)
        (message-writer output-port)
        (write-string suffix output-port))
      (let ((row #f)
            (column #f))
        (dynamic-wind
          (lambda ()
            (start-notification "...")
            (force-output tracker)
            (set! row (port-tracker-row tracker))
            (set! column (port-tracker-column tracker)))
          (lambda ()
            (parameterize ((notification-level (+ (notification-level) 1)))
              (thunk)))
          (lambda ()
            (if (not (and row
                          column
                          (= row (port-tracker-row tracker))
                          (= column (port-tracker-column tracker))))
                (start-notification " --"))
            (write-string " done" output-port)
            (newline output-port)
            (force-output tracker))))))

  
  ;;;; Cells
  
  (define-record-type (<cell> make-cell cell?)
    (fields
     (mutable value cell-ref cell-set!)))
  
)
