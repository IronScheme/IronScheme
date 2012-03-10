#!r6rs

(library (wak trc-testing display-condition)
  (export display-condition)
  (import (rnrs)
          (wak foof-loop)
          (wak fmt))
  
(define-syntax formatting
  (syntax-rules ()
    ((_ (state-var) (formatter state-expr) cont . env)
     (cont
      ()                                         ;Outer bindings
      ((state-var state-expr                     ;Loop variables
                  (formatter state-expr)))
      ()                                         ;Entry bindings
      ()                                         ;Termination conditions
      ()                                         ;Body bindings 
      ()                                         ;Final bindings
      . env))))

(define (dsp-simple-condition c)
  (define (dsp-rtd.fields-list rtd.fields-list n-fields)
    (case n-fields
      ((0) nl)
      ((1)
       (cat ": " (wrt/unshared ((record-accessor (caar rtd.fields-list) 0) c)) nl))
      (else
       (cat ":\n"
            (fmt-join
             (lambda (rtd.fields)
               (dsp-fields (car rtd.fields) (cdr rtd.fields)))
             rtd.fields-list
             "\n")))))
  (define (dsp-fields rtd fields)
    (lambda (st)
      (loop ((for i (up-from 0 (to (vector-length fields))))
             (for st (formatting
                      (cat "      "
                           (vector-ref fields i) ": "
                           (wrt/unshared ((record-accessor rtd i) c)) "\n")
                      st)))
        => st)))
  (lambda (st)
    (let ((c-rtd (record-rtd c)))
      (loop ((with rtd c-rtd (record-type-parent rtd))
             (while rtd)
             (for rtd.fields-list
                  (listing (cons rtd (record-type-field-names rtd))))
             (for n-fields (summing (vector-length
                                     (record-type-field-names rtd)))))
        => ((cat (record-type-name c-rtd)
                 (dsp-rtd.fields-list
                  (remp (lambda (rtd.fields)
                          (zero? (vector-length (cdr rtd.fields))))
                        rtd.fields-list)
                  n-fields))
            st)))))

(define (dsp-condition c)
  (define (dsp-components components)
    (lambda (st)
      (loop ((for c (in-list components))
             (for i (up-from 1))
             (for st (formatting
                      (cat "  " i ". " (dsp-simple-condition c))
                      st)))
        => st)))
  (cond
    ((condition? c)
     (let ((components (simple-conditions c)))
       (if (null? components)
           (dsp "Condition object with no further information\n")
           (cat "Condition components:\n"
                (dsp-components components)))))
    (else
     (cat "Non-condition object: " c (wrt/unshared c) "\n"))))

(define display-condition
  (case-lambda
    ((c port)
     (fmt port (fmt-columns (list (lambda (line) (cat " " line))
                                  (dsp-condition c)))))
    ((c)
     (display-condition c (current-output-port)))))

)
