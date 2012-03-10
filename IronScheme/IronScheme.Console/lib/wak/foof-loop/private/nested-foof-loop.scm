;;; -*- Mode: Scheme -*-

;;;; Nested Loops with foof-loop, Version 10 (BETA)

;;; Copyright (c) 2008, Taylor R. Campbell
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;; * Redistributions in binary form must reproduce the above copyright
;;;   notice, this list of conditions and the following disclaimer in
;;;   the documentation and/or other materials provided with the
;;;   distribution.
;;;
;;; * Neither the names of the authors nor the names of contributors
;;;   may be used to endorse or promote products derived from this
;;;   software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define-syntax nested-loop
  (syntax-rules ()
    ((NESTED-LOOP continuation ((state initial) ...) combiner
         clause0 clause1+ ...)
     (%NESTED-LOOP LOOP continuation ((state initial) ...) combiner
         clause0 clause1+ ...))))

(define-syntax nested-lazy-loop
  (syntax-rules ()
    ((NESTED-LOOP continuation ((state initial) ...) combiner
         clause0 clause1+ ...)
     (%NESTED-LOOP LAZY-LOOP continuation ((state initial) ...) combiner
         clause0 clause1+ ...))))

(define-syntax %nested-loop
  (syntax-rules (PARALLEL NESTED DO LET LET-VALUES IF NOT AND OR)

    ((%NESTED-LOOP looper continuation ((state initial) ...) combiner
       expression)
     (LET ((state initial) ...)
       (combiner (LAMBDA () expression) continuation)))

    ((%NESTED-LOOP looper continuation ((state initial) ...) combiner
         (PARALLEL (iterator ...) ...)
         clause0 clause1+ ...)
     (looper CONTINUE ((WITH state initial)
                       ...
                       (iterator ...)
                       ...)
       => (continuation state ...)
       (%NESTED-LOOP looper (LAMBDA (state ...) (CONTINUE state ...))
           ((state state) ...)
           combiner
           clause0 clause1+ ...)))

    ((%NESTED-LOOP looper continuation ((state initial) ...) combiner
         (NESTED clause ...)
         clause0 clause1+ ...)
     (%NESTED-LOOP looper continuation ((state initial) ...) combiner
         clause ... clause0 clause1+ ...))

    ((%NESTED-LOOP looper continuation ((state initial) ...) combiner
         (DO command ...)
         clause0 clause1+ ...)
     (BEGIN command ...
            (%NESTED-LOOP looper continuation ((state initial) ...) combiner
                clause0 clause1+ ...)))

    ((%NESTED-LOOP looper continuation ((state initial) ...) combiner
         (LET ((variable value) ...))
         clause0 clause1+ ...)
     (LET ((variable value) ...)
       (%NESTED-LOOP looper continuation ((state initial) ...) combiner
           clause0 clause1+ ...)))

    ((%NESTED-LOOP looper continuation ((state initial) ...) combiner
         (LET variable value)
         clause0 clause1+ ...)
     (LET ((variable value))
       (%NESTED-LOOP looper continuation ((state initial) ...) combiner
           clause0 clause1+ ...)))

    ((%NESTED-LOOP looper continuation ((state initial) ...) combiner
         (LET-VALUES ((bvl expression) ...))
         clause0 clause1+ ...)
     (LET-VALUES ((bvl expression) ...)
       (%NESTED-LOOP looper continuation ((state initial) ...) combiner
           clause0 clause1+ ...)))

    ((%NESTED-LOOP looper continuation ((state initial) ...) combiner
         (LET-VALUES bvl expression)
         clause0 clause1+ ...)
     (LET-VALUES ((bvl expression))
       (%NESTED-LOOP looper continuation ((state initial) ...) combiner
           clause0 clause1+ ...)))

    ((%NESTED-LOOP looper continuation ((state initial) ...) combiner
         (IF condition)
         clause0 clause1+ ...)
     (IF condition
         (%NESTED-LOOP looper continuation ((state initial) ...) combiner
             clause0 clause1+ ...)
         (continuation initial ...)))

    ((%NESTED-LOOP looper continuation ((state initial) ...) combiner
         ((iterator ...) ...)
         clause0 clause1+ ...)
     (%NESTED-LOOP looper continuation ((state initial) ...) combiner
         (PARALLEL (iterator ...) ...)
         clause0 clause1+ ...))

    ;** This clause must come last!  It would shadow the others.

    ((%NESTED-LOOP looper continuation ((state initial) ...) combiner
         (iterator ...)
         clause0 clause1+ ...)
     (%NESTED-LOOP looper continuation ((state initial) ...) combiner
         (PARALLEL (iterator ...))
         clause0 clause1+ ...))))

;;;; Iteration

(define-syntax iterate*
  (syntax-rules (=>)
    ((ITERATE* ((state initial) ...) => result stepper clause0 clause1+ ...)
     (NESTED-LOOP (LAMBDA (state ...) result)
         ((state initial) ...) stepper clause0 clause1+ ...))
    ((ITERATE* ((state initial) ...) stepper clause0 clause1+ ...)
     (NESTED-LOOP VALUES* ((state initial) ...) stepper
         clause0 clause1+ ...))))

(define-syntax iterate
  (syntax-rules (=>)
    ((ITERATE ((state initial) ...) => result stepper clause0 clause1+ ...)
     (ITERATE* ((state initial) ...) => result
         (LAMBDA (BODY CONTINUATION)
           (RECEIVE (state ...) (stepper (BODY) state ...)
             (CONTINUATION state ...)))
         clause0 clause1+ ...))
    ((ITERATE ((state initial) ...) stepper clause0 clause1+ ...)
     (ITERATE* ((state initial) ...)
         (LAMBDA (BODY CONTINUATION)
           (RECEIVE (state ...) (stepper (BODY) state ...)
             (CONTINUATION state ...)))
         clause0 clause1+ ...))))

(define-syntax iterate!
  (syntax-rules ()
    ((ITERATE! clause0 clause1+ ...)
     (ITERATE* ()                       ;No state
         (LAMBDA (BODY CONTINUATION) (BODY) (CONTINUATION))
         clause0 clause1+ ...))))

(define-syntax iterate-values
  (syntax-rules (=>)

    ((ITERATE-VALUES ((state initial) ...) => result
         clause0 clause1+ ...)
     (ITERATE* ((state initial) ...) => result CALL-WITH-VALUES
         clause0 clause1+ ...))

    ((ITERATE-VALUES updater ((state initial) ...) => result
         clause0 clause1+ ...)
     ;++ This should be visible only in the final expression.  However,
     ;++ that requires tail patterns, which are non-standard.
     (WITH-EXTENDED-PARAMETER-OPERATORS
         ((updater (VALUES* (state . state) ...)))
       (ITERATE-VALUES ((state initial) ...) => result clause0 clause1+ ...)))

    ((ITERATE-VALUES ((state initial) ...) clause0 clause1+ ...)
     (ITERATE* ((state initial) ...) CALL-WITH-VALUES
         clause0 clause1+ ...))

    ((ITERATE-VALUES updater ((state initial) ...) clause0 clause1+ ...)
     (WITH-EXTENDED-PARAMETER-OPERATORS
         ((updater (VALUES* (state . state) ...)))
       (ITERATE* ((state initial) ...) CALL-WITH-VALUES
         clause0 clause1+ ...)))))

;++ Hack for MIT Scheme, whose multiple return values are broken.

(define-syntax values*
  (syntax-rules ()
    ((VALUES* single) single)
    ((VALUES* multiple ...) (VALUES multiple ...))))

;;;; Recursion

(define-syntax recur*
  (syntax-rules ()
    ((RECUR* base-case combiner clause0 clause1+ ...)
     (NESTED-LOOP (LAMBDA () base-case)
         ()                             ;No state
         combiner
         clause0 clause1+ ...))))

(define-syntax lazy-recur*
  (syntax-rules ()
    ((LAZY-RECUR* base-case combiner clause0 clause1+ ...)
     (NESTED-LAZY-LOOP (LAMBDA () base-case)
         ()                             ;No state
         combiner
         clause0 clause1+ ...))))

(define-syntax recur
  (syntax-rules ()
    ((RECUR base-case combiner clause0 clause1+ ...)
     (RECUR* base-case
         (LAMBDA (BODY CONTINUATION)
           (combiner (BODY) (CONTINUATION)))
         clause0 clause1+ ...))))

(define-syntax lazy-recur
  (syntax-rules ()
    ((LAZY-RECUR base-case combiner clause0 clause1+ ...)
     (LAZY-RECUR* base-case
         (LAMBDA (BODY CONTINUATION)
           (combiner (BODY) (CONTINUATION)))
         clause0 clause1+ ...))))

(define-syntax recur-values
  (syntax-rules (=>)
    ((RECUR-VALUES base-case => result clause0 clause1+ ...)
     (CALL-WITH-VALUES (LAMBDA ()
                         (RECUR-VALUES base-case clause0 clause1+ ...))
       result))

    ((RECUR-VALUES base-case clause0 clause1+ ...)
     (RECUR* base-case
         (LAMBDA (RECEIVER-BODY RECURSION)
           (CALL-WITH-VALUES RECURSION (RECEIVER-BODY)))
         clause0 clause1+ ...))))

;;;; Collecting Lists & Streams

(define-syntax collect-list-reverse
  (syntax-rules (INITIAL)

    ((COLLECT-LIST-REVERSE (INITIAL tail-expression) clause0 clause1+ ...)
     (ITERATE ((TAIL tail-expression)) CONS clause0 clause1+ ...))

    ((COLLECT-LIST-REVERSE clause0 clause1+ ...)
     (COLLECT-LIST-REVERSE (INITIAL '()) clause0 clause1+ ...))))

;;; The first definition of COLLECT-LIST is probably the one that you
;;; want.  On the other hand, what follows in comments is elegant, and
;;; shows the flexibility of the mchanism, especially when compared
;;; with the definition of COLLECT-STREAM.

(define-syntax collect-list
  (syntax-rules (INITIAL)

    ((COLLECT-LIST (INITIAL tail-expression) clause0 clause1+ ...)
     (APPEND-REVERSE (COLLECT-LIST-REVERSE clause0 clause1+ ...)
                     tail-expression))

    ((COLLECT-LIST clause0 clause1+ ...)
     (REVERSE (COLLECT-LIST-REVERSE clause0 clause1+ ...)))))

; (define-syntax collect-list
;   (syntax-rules (INITIAL)
;
;     ((COLLECT-LIST (INITIAL tail-expression) clause0 clause1+ ...)
;      (RECUR tail-expression CONS clause0 clause1+ ...))
;
;     ((COLLECT-LIST clause0 clause1+ ...)
;      (COLLECT-LIST (INITIAL '()) clause0 clause1+ ...))))

(define-syntax collect-stream
  (syntax-rules (INITIAL)

    ((COLLECT-STREAM (INITIAL tail-expression) clause0 clause1+ ...)
     (LAZY-RECUR tail-expression STREAM-CONS clause0 clause1+ ...))

    ((COLLECT-STREAM clause0 clause1+ ...)
     (COLLECT-STREAM (INITIAL STREAM-NIL) clause0 clause1+ ...))))

(define-syntax collect-list!
  (syntax-rules (INITIAL)

    ((COLLECT-LIST! (INITIAL tail-expression) clause0 clause1+ ...)
     (LET ((PAIR (CONS #F tail-expression)))
       (COLLECT-LIST-INTO! PAIR clause0 clause1+ ...)
       (CDR PAIR)))

    ((COLLECT-LIST! clause0 clause1+ ...)
     (COLLECT-LIST! (INITIAL '()) clause0 clause1+ ...))))

(define-syntax collect-list-into!
  (syntax-rules ()
    ((COLLECT-LIST-INTO! pair-expression clause0 clause1+ ...)
     (ITERATE* ((PAIR pair-expression))
         (LAMBDA (BODY CONTINUATION)
           (LET ((TAIL (CONS (BODY) (CDR PAIR))))
             (SET-CDR! PAIR TAIL)
             (CONTINUATION TAIL)))
         clause0 clause1+ ...))))

;;;; Collecting Vectors and Strings

(define-syntax collect-vector
  (syntax-rules ()
    ((COLLECT-VECTOR clause0 clause1+ ...)
     (LIST->VECTOR (COLLECT-LIST clause0 clause1+ ...)))))

(define-syntax collect-string
  (syntax-rules ()
    ((COLLECT-STRING clause0 clause1+ ...)
     (LIST->STRING (COLLECT-LIST clause0 clause1+ ...)))))

;;; The following definition of COLLECT-DISPLAY can collect any object,
;;; whose printed representation is computed using DISPLAY; it relies
;;; on SRFI 6 (Basic String Ports) to accomplish this.

(define-syntax collect-display
  (syntax-rules ()
    ((COLLECT-DISPLAY clause0 clause1+ ...)
     (LET ((OUTPUT-PORT (OPEN-OUTPUT-STRING)))
       (ITERATE* ()                      ;No state
           (LAMBDA (BODY CONTINUATION)
             (DISPLAY (BODY) OUTPUT-PORT)
             (CONTINUATION))
           clause0 clause1+ ...)
       (GET-OUTPUT-STRING OUTPUT-PORT)))))

;;;;; Expanding Vector and String Collection

;;; These are slower than the definitions with lists.  Go figure.

; (define-syntax collect-vector
;   (syntax-rules ()
;     ((COLLECT-VECTOR clause0 clause1+ ...)
;      (%COLLECT-VECTOR
;       (MAKE-VECTOR VECTOR-LENGTH VECTOR-SET! IN-VECTOR)
;       DATUM
;       ()           ;No check for the data.
;       clause0 clause1+ ...))))
;
; (define-syntax collect-string
;   (syntax-rules ()
;     ((COLLECT-STRING clause0 clause1+ ...)
;      (%COLLECT-VECTOR
;       (MAKE-STRING STRING-LENGTH STRING-SET! IN-STRING)
;       DATUM
;       ((IF (NOT (CHAR? DATUM))
;            (ERROR "Non-character in COLLECT-STRING:" DATUM)))
;       clause0 clause1+ ...))))
;
; (define-syntax %collect-vector
;   (syntax-rules ()
;     ((%COLLECT-VECTOR
;       (make-vector vector-length vector-set! in-vector)
;       datum (check ...)
;       clause0 clause1+ ...)
;      (RECEIVE (LENGTH CHUNK-INDEX CHUNK CHUNKS)
;          (ITERATE ((LENGTH 0)
;                    (CHUNK-INDEX 0)
;                    (CHUNK (make-vector #x10))
;                    (CHUNKS '()))
;              (LAMBDA (datum LENGTH CHUNK-INDEX CHUNK CHUNKS)
;                check ...
;                (LET ((CHUNK-LENGTH (vector-length CHUNK)))
;                  (IF (< CHUNK-INDEX CHUNK-LENGTH)
;                      (BEGIN
;                        (vector-set! CHUNK CHUNK-INDEX datum)
;                        (VALUES LENGTH
;                                (+ CHUNK-INDEX 1)
;                                CHUNK
;                                CHUNKS))
;                      (LET ((CHUNK*
;                             (make-vector
;                              (IF (>= CHUNK-LENGTH #x1000)
;                                  #x1000
;                                  (* CHUNK-LENGTH 2)))))
;                        (vector-set! CHUNK* 0 datum)
;                        (VALUES (+ LENGTH CHUNK-LENGTH)
;                                1        ;We filled in the first slot,
;                                CHUNK*   ;  so start at index 1.
;                                (CONS CHUNK CHUNKS))))))
;              clause0 clause1+ ...)
;        (LET* ((TOTAL-LENGTH (+ LENGTH CHUNK-INDEX))
;               (RESULT (make-vector TOTAL-LENGTH)))
;          (LOOP ((FOR ELEMENT OFFSET (in-vector CHUNK 0 CHUNK-INDEX)))
;            (vector-set! RESULT (+ LENGTH OFFSET) ELEMENT))
;          (LOOP ((FOR CHUNK (IN-LIST CHUNKS))
;                 (WITH BASE LENGTH BASE*)
;                 (LET BASE* (- BASE (vector-length CHUNK))))
;            (LOOP ((FOR ELEMENT OFFSET (in-vector CHUNK)))
;              (vector-set! RESULT (+ BASE* OFFSET) ELEMENT)))
;          RESULT)))))

;;;;; Non-reentrant Vector and String Collection

;;; For the following definitions, we defer the responsibility of
;;; bounds checking and error signalling to VECTOR-SET! and
;;; STRING-SET!.  This may not be a good idea.

(define-syntax collect-into-vector!
  (syntax-rules (FROM)

    ((COLLECT-INTO-VECTOR! vector-expression (FROM start-expression)
       clause0 clause1+ ...)
     (LET ((VECTOR vector-expression)
           (START start-expression))
       (ITERATE* ((INDEX start))
           (LAMBDA (BODY CONTINUATION)
             (VECTOR-SET! VECTOR INDEX (BODY))
             (CONTINUATION (+ INDEX 1)))
           clause0 clause1+ ...)))

    ((COLLECT-INTO-VECTOR! vector-expression clause0 clause1+ ...)
     (COLLECT-INTO-VECTOR! vector-expression (FROM 0) clause0 clause1+ ...))))

(define-syntax collect-into-string!
  (syntax-rules (FROM)

    ((COLLECT-INTO-STRING! string-expression (FROM start-expression)
       clause0 clause1+ ...)
     (LET ((STRING string-expression)
           (START start-expression))
       (ITERATE* ((INDEX start))
           (LAMBDA (BODY CONTINUATION)
             (STRING-SET! STRING INDEX (BODY))
             (CONTINUATION (+ INDEX 1)))
           clause0 clause1+ ...)))

    ((COLLECT-INTO-STRING! string-expression clause0 clause1+ ...)
     (COLLECT-INTO-STRING! string-expression (FROM 0) clause0 clause1+ ...))))

;;; These should probably have bang suffixes to emphasize that they are
;;; non-reentrant.

(define-syntax collect-vector-of-length
  (syntax-rules ()
    ((COLLECT-VECTOR-OF-LENGTH length clause0 clause1+ ...)
     (LET ((VECTOR (MAKE-VECTOR length)))
       (COLLECT-INTO-VECTOR! VECTOR clause0 clause1+ ...)
       VECTOR))))

(define-syntax collect-string-of-length
  (syntax-rules ()
    ((COLLECT-STRING-OF-LENGTH length clause0 clause1+ ...)
     (LET ((STRING (MAKE-STRING length)))
       (COLLECT-INTO-STRING! STRING clause0 clause1+ ...)
       STRING))))

;;;; Numerical Collection

(define-syntax collect-sum
  (syntax-rules (INITIAL)

    ((COLLECT-SUM (INITIAL value-expression) clause0 clause1+ ...)
     (ITERATE ((SUM value-expression)) + clause0 clause1+ ...))

    ((COLLECT-SUM clause0 clause1+ ...)
     (COLLECT-SUM (INITIAL 0) clause0 clause1+ ...))))

(define-syntax collect-product
  (syntax-rules (INITIAL)

    ((COLLECT-PRODUCT (INITIAL value-expression) clause0 clause1+ ...)
     (ITERATE ((PRODUCT value-expression)) * clause0 clause1+ ...))

    ((COLLECT-PRODUCT clause0 clause1+ ...)
     (COLLECT-PRODUCT (INITIAL 1) clause0 clause1+ ...))))

(define-syntax collect-count
  (syntax-rules ()
    ((COLLECT-COUNT clause0 clause1+ ...)
     (COLLECT-SUM clause0 clause1+ ... 1))))

(define-syntax collect-average
  (syntax-rules ()
    ((COLLECT-AVERAGE clause0 clause1+ ...)
     (RECEIVE (SUM COUNT)
              (ITERATE* ((SUM 0) (COUNT 0))
                  (LAMBDA (BODY CONTINUATION)
                    (CONTINUATION (+ SUM (BODY)) (+ COUNT 1)))
                  clause0 clause1+ ...)
       (/ SUM COUNT)))))

;;;; Collecting Extrema

(define-syntax collect-extremum
  (syntax-rules (INITIAL)

    ((COLLECT-EXTREMUM comparator-expression (INITIAL initial-expression)
         clause0 clause1+ ...)
     (LET ((COMPARATOR comparator-expression))
       (ITERATE ((EXTREMUM initial-expression))
           (LAMBDA (DATUM EXTREMUM)
             (IF (COMPARATOR DATUM EXTREMUM) DATUM EXTREMUM))
           clause0 clause1+ ...)))

    ((COLLECT-EXTREMUM comparator-expression clause0 clause1+ ...)
     (LET ((COMPARATOR comparator-expression))
       (ITERATE ((EXTREMUM #F))
           (LAMBDA (DATUM EXTREMUM)
             (IF (AND DATUM EXTREMUM)
                 (IF (COMPARATOR DATUM EXTREMUM) DATUM EXTREMUM)
                 (OR DATUM EXTREMUM)))
         clause0 clause1+ ...)))))

(define-syntax collect-minimum
  (syntax-rules (INITIAL)

    ((COLLECT-MINIMUM (INITIAL initial-expression) clause0 clause1+ ...)
     (ITERATE ((MINIMUM initial-expression)) MIN clause0 clause1+ ...))

    ((COLLECT-MINIMUM clause0 clause1+ ...)
     (ITERATE ((MINIMUM #F))
         (LAMBDA (DATUM MINIMUM)
           (IF (AND DATUM MINIMUM)
               (MIN DATUM MINIMUM)
               (OR DATUM MINIMUM)))
         clause0 clause1+ ...))))

(define-syntax collect-maximum
  (syntax-rules (INITIAL)

    ((COLLECT-MAXIMUM (INITIAL initial-expression) clause0 clause1+ ...)
     (ITERATE ((MAXIMUM initial-expression)) MAX clause0 clause1+ ...))

    ((COLLECT-MAXIMUM clause0 clause1+ ...)
     (ITERATE ((MAXIMUM #F))
         (LAMBDA (DATUM MAXIMUM)
           (IF (AND DATUM MAXIMUM)
               (MAX DATUM MAXIMUM)
               (OR DATUM MAXIMUM)))
         clause0 clause1+ ...))))

;;;;; Generalization by Multiple Values

(define-syntax collect-extremum*
  (syntax-rules (INITIAL)

    ((COLLECT-EXTREMUM* comparator-expression
         (INITIAL key-expression element-expression)
         clause0 clause1+ ...)
     (LET ((COMPARATOR comparator-expression)
           (INITIAL-KEY key-expression)
           (INITIAL-ELEMENT element-expression))
       (ITERATE* ((EXTREME-KEY INITIAL-KEY)
                  (EXTREME-ELEMENT INITIAL-ELEMENT))
           (LAMBDA (BODY CONTINUATION)
             (RECEIVE (KEY ELEMENT) (BODY)
               (IF (COMPARATOR KEY EXTREME-KEY)
                   (CONTINUATION KEY ELEMENT)
                   (CONTINUATION EXTREME-KEY EXTREME-ELEMENT))))
           clause0 clause1+ ...)))

    ((COLLECT-EXTREMUM* comparator-expression clause0 clause1+ ...)
     (LET ((COMPARATOR comparator-expression))
       (ITERATE* ((EXTREME-KEY #F)
                  (EXTREME-ELEMENT #F))
           (LAMBDA (BODY CONTINUATION)
             (RECEIVE (KEY ELEMENT) (BODY)
               (IF KEY
                   (IF EXTREME-KEY
                       (IF (COMPARATOR KEY EXTREME-KEY)
                           (CONTINUATION KEY ELEMENT)
                           (CONTINUATION EXTREME-KEY EXTREME-ELEMENT))
                       (CONTINUATION KEY ELEMENT))
                   (CONTINUATION EXTREME-KEY EXTREME-ELEMENT))))
           clause0 clause1+ ...)))))

(define-syntax collect-minimum*
  (syntax-rules (INITIAL)

    ((COLLECT-MINIMUM* (INITIAL key-expression element-expression)
         clause0 clause1+ ...)
     (COLLECT-EXTREMUM* < (INITIAL key-expression element-expression)
         clause0 clause1+ ...))

    ((COLLECT-MINIMUM* clause0 clause1+ ...)
     (COLLECT-EXTREMUM* < clause0 clause1+ ...))))

(define-syntax collect-maximum*
  (syntax-rules (INITIAL)

    ((COLLECT-MAXIMUM* (INITIAL key-expression element-expression)
         clause0 clause1+ ...)
     (COLLECT-EXTREMUM* < (INITIAL key-expression element-expression)
         clause0 clause1+ ...))

    ((COLLECT-MAXIMUM* clause0 clause1+ ...)
     (COLLECT-EXTREMUM* < clause0 clause1+ ...))))

;;;;; Generalization by Selectors

(define-syntax collect-extremum-by
  (syntax-rules (INITIAL)

    ((COLLECT-EXTREMUM-BY comparator-expression selector-expression
         (INITIAL initial-expression)
         clause0 clause1+ ...)
     (LET ((COMPARATOR comparator-expression)
           (SELECTOR selector-expression)
           (INITIAL-ELEMENT initial-expression))
       (ITERATE* ((EXTREME-KEY (SELECTOR INITIAL-ELEMENT))
                  (EXTREME-ELEMENT INITIAL-ELEMENT))
           => EXTREME-ELEMENT
           (LAMBDA (BODY CONTINUATION)
             (LET* ((ELEMENT (BODY))
                    (KEY (SELECTOR ELEMENT)))
               (IF (COMPARATOR KEY EXTREME-KEY)
                   (CONTINUATION KEY ELEMENT)
                   (CONTINUATION EXTREME-KEY EXTREME-ELEMENT))))
           clause0 clause1+ ...)))

    ((COLLECT-EXTREMUM-BY comparator-expression selector-expression
         clause0 clause1+ ...)
     (LET ((COMPARATOR comparator-expression)
           (SELECTOR selector-expression))
       (ITERATE* ((EXTREME-KEY #F) (EXTREME-ELEMENT #F))
           => EXTREME-ELEMENT
           (LAMBDA (BODY CONTINUATION)
             (LET* ((ELEMENT (BODY))
                    (KEY (SELECTOR ELEMENT)))
               (IF KEY
                   (IF EXTREME-KEY
                       (IF (COMPARATOR KEY EXTREME-KEY)
                           (CONTINUATION KEY ELEMENT)
                           (CONTINUATION EXTREME-KEY EXTREME-ELEMENT))
                       (CONTINUATION KEY ELEMENT))
                   (CONTINUATION EXTREME-KEY EXTREME-ELEMENT))))
           clause0 clause1+ ...)))))

(define-syntax collect-minimum-by
  (syntax-rules (INITIAL)

    ((COLLECT-MINIMUM-BY selector-expression (INITIAL initial-expression)
         clause0 clause1+ ...)
     (COLLECT-EXTREMUM-BY < selector-expression (INITIAL initial-expression)
         clause0 clause1+ ...))

    ((COLLECT-MINIMUM-BY selector-expression clause0 clause1+ ...)
     (COLLECT-EXTREMUM-BY < selector-expression clause0 clause1+ ...))))

(define-syntax collect-maximum-by
  (syntax-rules (INITIAL)

    ((COLLECT-MAXIMUM-BY selector-expression (INITIAL initial-expression)
         clause0 clause1+ ...)
     (COLLECT-EXTREMUM-BY > selector-expression (INITIAL initial-expression)
         clause0 clause1+ ...))

    ((COLLECT-MAXIMUM-BY selector-expression clause0 clause1+ ...)
     (COLLECT-EXTREMUM-BY > selector-expression clause0 clause1+ ...))))

;;;; Miscellaneous

;;; COLLECT-FIRST and COLLECT-OR work nicely.  COLLECT-LAST and
;;; COLLECT-AND have the unfortunate property that the final expression
;;; is not evaluated in a tail position, which is very hard to arrange
;;; in the general case.  For example, compare these two definitions of
;;; (a reduced version of) EVERY from SRFI 1:
;;;
;;; (define (every predicate list)
;;;   (and (pair? list)
;;;        (let loop ((list list))
;;;          (let ((tail (cdr list)))
;;;            (if (pair? tail)
;;;                (and (predicate (car list))
;;;                     (loop tail))
;;;                (predicate (car list)))))))
;;;
;;; (define (every predicate list)
;;;   (collect-and (for element (in-list list))
;;;     (predicate element)))
;;;
;;; The first definition duplicates the call to PREDICATE so that the
;;; last is in a tail position.  COLLECT-AND cannot do this.

(define-syntax collect-first
  (syntax-rules (DEFAULT)

    ((COLLECT-FIRST (DEFAULT default-expression) clause0 clause1+ ...)
     (NESTED-LOOP (LAMBDA () default-expression)
         ()                             ;No state
         (LAMBDA (BODY CONTINUATION)
           CONTINUATION                 ;ignore
           (BODY))
         clause0 clause1+ ...))

    ((COLLECT-FIRST clause0 clause1+ ...)
     (COLLECT-FIRST (DEFAULT (ERROR "Nothing generated in COLLECT-FIRST."))
         clause0 clause1+ ...))))

(define-syntax collect-last
  (syntax-rules (DEFAULT)
    ((COLLECT-LAST (DEFAULT default-expression) clause0 clause1+ ...)
     (NESTED-LOOP (LAMBDA (RESULT) RESULT)
         ((RESULT default-expression))
         (LAMBDA (BODY CONTINUATION) (CONTINUATION (BODY)))
         clause0 clause1+ ...))))

(define-syntax collect-or
  (syntax-rules ()
    ((COLLECT-OR clause0 clause1+ ...)
     (NESTED-LOOP (LAMBDA () #F)
         ()                             ;No state
         (LAMBDA (BODY CONTINUATION) (OR (BODY) (CONTINUATION)))
         clause0 clause1+ ...))))

(define-syntax collect-and
  (syntax-rules ()
    ((COLLECT-AND clause0 clause1+ ...)
     (NESTED-LOOP (LAMBDA (RESULT) RESULT)
         ((RESULT #F))
         (LAMBDA (BODY CONTINUATION)
           (LET ((RESULT (BODY))) (AND RESULT (CONTINUATION RESULT))))
         clause0 clause1+ ...))))
