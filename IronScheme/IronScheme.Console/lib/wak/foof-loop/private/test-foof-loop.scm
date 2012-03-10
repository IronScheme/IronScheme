;;; -*- Mode: Scheme -*-

;;;; Extensible Looping Macros
;;;; Test Suite

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define open-input-string open-string-input-port)
(define (identity-procedure x) x)

(define-test-suite loop-tests
  "Taylor R. Campbell's and Alex Shinn's loop macros")

(define-test-suite (loop-tests.null-loop loop-tests)
  "Loops that do nothing")

(define-test-case loop-tests.null-loop no-recursive-call ()
  (test-eqv 0 (loop continue () 0)))

(define-test-case loop-tests.null-loop loop-until-true ()
  (test-eqv 0 (loop ((until #t)) => 0)))

(define-test-suite (loop-tests.trivial-do loop-tests)
  "Loops trivially translated from DO")

(define-test-case loop-tests.trivial-do iota ()
  (test-equal '(0 1 2)
    (loop continue ((integer 0 (+ integer 1))
                    (list '() (cons integer list)))
      (if (= integer 3)
          (reverse list)
          (continue)))))

(define-test-case loop-tests.trivial-do list-sum ()
  (test-eqv 25
    ;; R5RS, 4.2.4, p12, translated from DO.
    (let ((x '(1 3 5 7 9)))
      (loop ((x x (cdr x))
             (sum 0 (+ sum (car x)))
             (until (null? x)))
        => sum))))

(define-test-suite (loop-tests.trivial-named-let loop-tests)
  "Loops trivially translated from named LET")

(define-test-case loop-tests.trivial-named-let successive-sum ()
  (test-eqv 45
    (loop continue ((i 0) (sum 0))
      (if (= i 10)
          sum
          (continue (+ i 1) (+ sum i))))))

(define-test-case loop-tests.trivial-named-let partition-list-by-sign ()
  (test-equal '((6 1 3) (-5 -2))
    ;; R5RS, 4.2.4, p12, translated from named LET.
    (loop continue ((numbers '(3 -2 1 6 -5))
                    (nonneg '())
                    (neg '()))
      (cond ((null? numbers) (list nonneg neg))
            ((>= (car numbers) 0)
             (continue (cdr numbers)
                       (cons (car numbers) nonneg)
                       neg))
            ((< (car numbers) 0)
             (continue (cdr numbers)
                       nonneg
                       (cons (car numbers) neg)))))))

(define-test-suite (loop-tests.in-list loop-tests)
  "IN-LIST iterator")

(define-test-case loop-tests.in-list sum ()
  (test-eqv 6
    (loop ((for element (in-list '(1 2 3)))
           (with sum 0 (+ sum element)))
      => sum)))

(define-test-case loop-tests.in-list reverse ()
  (test-equal '(2 1 0)
    (loop ((for element (in-list '(0 1 2)))
           (with reversed '() (cons element reversed)))
      => reversed)))

(define-test-case loop-tests.in-list find-matching-items ()
  (test-equal '(-4 #F FOO)
    (let ((items '(3 -1.2 -4 1 FOO 9 2 6 FROTZ)))
      (define (find-matching-item list predicate)
        (loop continue ((for item (in-list list)))
          => #f
          (if (predicate item) item (continue))))
      (list (find-matching-item items
              (lambda (item) (and (integer? item) (negative? item))))
            (find-matching-item items pair?)
            (find-matching-item items symbol?)))))

(define-test-case loop-tests.in-list pairwise-sum ()
  (test-equal '(5 7 9)
    (loop ((for a (in-list '(1 2 3)))
           (for b (in-list '(4 5 6)))
           (with pairwise-sum '() (cons (+ a b) pairwise-sum)))
      => (reverse pairwise-sum))))

(define-test-case loop-tests.in-list plist->alist ()
  (test-equal '((:X FOO) (:Y BAR) (:Z BAZ))
    (loop ((for key tail (in-list '(:X FOO :Y BAR :Z BAZ) cddr))
           (with alist '() (cons (list key (cadr tail)) alist)))
      => (reverse alist))))

(define-test-case loop-tests.in-list partition ()
  (test-equal '((4 2 6) (3 1 1 5 9 5))
    (loop continue ((for element (in-list '(3 1 4 1 5 9 2 6 5)))
                    (with even '())
                    (with odd '()))
      => (list (reverse even) (reverse odd))
      (if (even? element)
          (continue (=> even (cons element even)))
          (continue (=> odd (cons element odd)))))))

(define-test-case loop-tests.in-list inner-product ()
  (test-equal 32
    (loop ((for components (in-lists '((1 2 3) (4 5 6))))
           (with inner-product 0
             (+ inner-product
                (loop ((for component (in-list components))
                       (with product 1 (* product component)))
                  => product))))
      => inner-product)))

(define-test-case loop-tests.in-list matrix-transposition ()
  (test-equal '((C F) (B E) (A D))
    (loop ((for columns (in-lists '((A B C) (D E F))))
           (with rows '() (cons columns rows)))
      => rows)))

(define-test-suite (loop-tests.in-vector loop-tests)
  "IN-VECTOR and IN-VECTOR-REVERSE iterator")

(define-test-case loop-tests.in-vector sum ()
  (test-eqv 20
    (loop ((for element (in-vector '#(2 4 6 8)))
           (with sum 0 (+ sum element)))
      => sum)))

(define-test-case loop-tests.in-vector vector-suffix->list ()
  (test-equal '(4 1 5 9)
    (loop ((for element (in-vector '#(3 1 4 1 5 9) 2))
           (with list '() (cons element list)))
      => (reverse list))))

(define-test-case loop-tests.in-vector subvector->list ()
  (test-equal '(1 4 1 5)
    (loop ((for element (in-vector-reverse '#(3 1 4 1 5 9) 5 1))
           (with list '() (cons element list)))
      => list)))

(define-test-case loop-tests.in-vector reverse-subvector->list ()
  (test-equal '(5 1 4 1)
    (loop ((for element (in-vector '#(3 1 4 1 5 9) 1 5))
           (with list '() (cons element list)))
      => list)))

(define-test-case loop-tests.in-vector linear-search ()
  (test-equal 2
    (loop continue ((for element index (in-vector '#(FOO BAR BAZ QUUX))))
      (if (eq? element 'BAZ)
          index
          (continue)))))

(define-test-case loop-tests.in-vector vector-reverse ()
  (test-equal '#(E D C)
    (let ()
      (define (vector-reverse-copy vector start end)
        (let* ((length (- end start))
               (vector* (make-vector length)))
          (loop ((for element index (in-vector vector start end)))
            (vector-set! vector* (- (- end 1) index) element))
          vector*))
      (vector-reverse-copy '#(A B C D E F G H I) 2 5))))

(define-test-case loop-tests.in-vector accelerated-alphabetic-traversal ()
  (test-equal '((A 0) (B 1) (D 3) (H 7) (P 15))
    ((lambda (body)
       (body '#(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)))
     (lambda (alphabet-vector)
       (loop continue ((for element index (in-vector alphabet-vector))
                       (with result '()
                         (cons (list element index) result)))
         => (reverse result)
         (continue (=> index (+ 1 (* 2 index)))))))))

(define-test-suite (loop-tests.in-string loop-tests)
  "IN-STRING and IN-STRING-REVERSE iterators")

(define-test-case loop-tests.in-string linear-search ()
  (test-eqv 4
    (loop continue ((for char index (in-string "foobarbaz")))
      (if (char=? char #\a)
          index
          (continue)))))

(define-test-case loop-tests.in-string linear-search-reverse ()
  (test-eqv 7
    (loop continue ((for char index (in-string-reverse "foobarbaz")))
      => #f
      (if (char=? char #\a)
          index
          (continue)))))

(define-test-case loop-tests.in-string string->list ()
  (test-equal '(#\o #\o #\b #\a)
    (loop ((for char (in-string "foobar" 1 5))
           (with chars '() (cons char chars)))
      => (reverse chars))))

(define-test-suite (loop-tests.io-loops loop-tests)
  "IN-PORT and IN-FILE iterators")

(define-test-case loop-tests.io-loops read-chars ()
  (test-equal '(#\x #\y #\z)
    (loop ((for char (in-port (open-input-string "xyz")))
           (with chars '() (cons char chars)))
      => (reverse chars))))

(define-test-case loop-tests.io-loops read-expressions ()
  (test-equal '(foo bar (baz quux) #(zot))
    (loop ((for expression
                (in-port (open-input-string "foo bar (baz quux) #(zot)")
                         read))
           (with expressions '() (cons expression expressions)))
      => (reverse expressions))))

(define-test-case loop-tests.io-loops read-with-custom-eof ()
  (test-equal '(foo bar baz)
    (loop ((for term
                (in-port (open-input-string "foo bar baz eof quux")
                         read
                         (lambda (term)
                           (if (eof-object? term)
                               (test-failure "Premature real EOF.")
                               (eq? term 'eof)))))
           (with terms '() (cons term terms)))
      => (reverse terms))))

;++ This should test IN-FILE, but we need some sort of temporary file
;++ generation utility, and to implement setup & teardown actions.

(define-test-suite (loop-tests.integer-intervals loop-tests)
  "UP-FROM and DOWN-FROM iterators")

(define-test-case loop-tests.integer-intervals successive-sum ()
  (test-equal 45
    (loop ((for i (up-from 0 (to 10)))
           (with sum 0 (+ sum i)))
      => sum)))

(define-test-case loop-tests.integer-intervals reverse-iota-evens ()
  (test-equal '(8 6 4 2 0)
    (loop ((for i (up-from 0 (to 10) (by 2)))
           (with list '() (cons i list)))
      => list)))

(define-test-case loop-tests.integer-intervals iota-odds ()
  (test-equal '(1 3 5 7 9)
    (loop ((for i (down-from 11 (to 1) (by 2)))
           (with list '() (cons i list)))
      => list)))

(define-test-case loop-tests.integer-intervals list-of-squares ()
  (test-equal '(0 1 4 9 16)
    (loop ((for i (down-from 5 (to 0)))
           (with squares '() (cons (* i i) squares)))
      => squares)))

(define-test-case loop-tests.integer-intervals sieve-of-eratosthenes ()
  (test-equal '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59)
    (let ()
      (define (make-bit-string size set?)
        (make-string size (if set? #\1 #\0)))
      (define (bit-string-set! bit-string index)
        (string-set! bit-string index #\1))
      (define (bit-string-clear! bit-string index)
        (string-set! bit-string index #\0))
      (define (bit-string-set? bit-string index)
        (char=? #\1 (string-ref bit-string index)))

      (define (sieve n)
        (let ((prime-table (make-bit-string (- n 2) #t)))
          (define (prime? k) (bit-string-set? prime-table (- k 2)))
          (define (not-prime! k)
            (bit-string-clear! prime-table (- k 2)))
          (define (prime! k)
            (loop ((for i (up-from (* k k) (to n) (by k))))
              (not-prime! i)))
          (loop ((for k (up-from 2 (to n)))
                 (with prime-list '()
                   (if (prime? k)
                       (begin (prime! k) (cons k prime-list))
                       prime-list)))
            => (reverse prime-list))))

      (sieve 60))))

(define-test-suite (loop-tests.accumulation loop-tests)
  "Accumulation iterators")

(define-test-case loop-tests.accumulation iota ()
  (test-equal '(0 1 2 3 4 5 6 7 8 9)
    (loop ((for i (up-from 0 (to 10)))
           (for list (listing i)))
      => list)))

(define-test-case loop-tests.accumulation append ()
  (test-equal '(a b c d e f)
    (let ()
      (define (append list tail)
        (loop ((for element (in-list list))
               (for tail (listing (initial tail) element)))
          => tail))
      (append '(a b c) '(d e f)))))

(define-test-case loop-tests.accumulation append-reverse ()
  (test-equal '(f e d c b a)
    (let ()
      (define (append-reverse list tail)
        (loop ((for element (in-list list))
               (for tail (listing-reverse (initial tail) element)))
          => tail))
      (append-reverse '(d e f) '(c b a)))))

(define-test-case loop-tests.accumulation iota-reverse ()
  (test-equal '(9 8 7 6 5 4 3 2 1 0)
    (loop ((for i (up-from 0 (to 10)))
           (for list (listing-reverse i)))
      => list)))

(define-test-case loop-tests.accumulation non-reentrant-map ()
  (test-equal '(1 4 9 16 25)
    (loop ((for i (in-list '(1 2 3 4 5)))
           (for squares (listing! (* i i))))
      => squares)))

(define-test-case loop-tests.accumulation even-product-iota! ()
  (test-equal '(INITIAL 0 4 16 36 64)
    (let ((x (cons 'INITIAL '())))
      (loop ((for i (up-from 0 (to 10)))
             (for result (listing-into! x (* i i) (if (even? i))))))
      x)))

(define-test-case loop-tests.accumulation concatenate ()
  (test-equal '(A B C P Q R 0 1 2)
    (loop ((for list (in-list '((A B C) (P Q R) (0 1 2))))
           (for result (appending list)))
      => result)))

(define-test-case loop-tests.accumulation reverse-concatenate ()
  (test-equal '(2 1 0 R Q P C B A)
    (loop ((for list (in-list '((A B C) (P Q R) (0 1 2))))
           (for result (appending-reverse list)))
      => result)))

(define-test-case loop-tests.accumulation maximize-if-even ()
  (test-equal 6
    (loop ((for i (in-list '(3 1 4 1 5 9 2 6 5 3 5)))
           (for j (maximizing i (if (even? i)))))
      => j)))

(define-test-case loop-tests.accumulation minimize-if-odd ()
  (test-equal 1
    (loop ((for i (in-list '(3 1 4 1 5 9 2 6 5 3 5)))
           (for j (minimizing i (if (odd? i)))))
      => j)))

(define-test-case loop-tests.accumulation sum-of-squares-of-valid-numbers ()
  (test-equal 1300
    (loop ((for string (in-list '("a" "12" "x" "34")))
           (for sum (summing (string->number string)
                             => (lambda (number) (* number number)))))
      => sum)))

(define-test-case loop-tests.accumulation sum-of-valid-even-numbers ()
  (test-equal 24
    (loop ((for string (in-list '("a" "2" "3" "6" "b" "16" "17" "x" "19")))
           (for sum (summing (values (string->number string))
                             (lambda (x) (and x (even? x)))
                             => (lambda (number) number))))
      => sum)))

(define-test-case loop-tests.accumulation factorial ()
  (test-equal 720
    (loop ((for i (up-from 1 (to (+ 6 1))))
           (for factorial (multiplying i)))
      => factorial)))

(define-test-suite (loop-tests.misc loop-tests)
  "Miscellaneous loops")

(define-test-case loop-tests.misc obfuscated-loop-invocation ()
  (test-equal '((0 () i (i j k p q r))
                (1 (0) k (k p q r))
                (2 (1 0) q (q r)))
    (loop continue ((with a 0)
                    (with b '() (cons a b))
                    (for c d (in-list '(i j k p q r)))
                    (for result (listing (list a b c d))))
      => result
      (continue (+ a 1)
                (=> d (cddr d))))))

(define-test-case loop-tests.misc vector-quick-sort ()
  (let ()
    (define (vector-copy vector)
      (let* ((length (vector-length vector))
             (vector* (make-vector length)))
        (loop ((for element index (in-vector vector)))
          (vector-set! vector* index element))
        vector*))
    (loop ((for vector (in-list '(#(A B C 8 6 5 3 1 4 0 7 2 9 D E F)
                                  #(A B C 2 7 4 9 3 6 8 5 0 1 D E F)
                                  #(A B C 0 8 9 3 5 4 6 1 7 2 D E F)
                                  #(A B C 7 8 3 0 2 1 4 6 9 5 D E F)
                                  #(A B C 9 7 4 8 3 0 1 2 5 6 D E F)
                                  #(A B C 1 9 2 6 4 7 3 8 0 5 D E F)))))
      (let ((vector (vector-copy vector)))
        (vector-quick-sort! vector 3 (- (vector-length vector) 3)
                            identity-procedure
                            <)
        (if (not (equal? vector '#(A B C 0 1 2 3 4 5 6 7 8 9 D E F)))
            (test-failure "Vector quick-sort yielded an unsorted vector:"
                          vector))))))

(define (vector-quick-sort! vector start end key-selector key<)
  (define (select-pivot vector start end)
    (vector-ref vector (quotient (+ start end) 2)))
  (loop sort ((start start) (end end))
    (if (< 1 (- end start))
        (let ((pivot (key-selector (select-pivot vector start end))))
          (loop continue ((i start) (j end))
            (let ((i (loop scan ((for i (up-from i)))
                       (if (key< (key-selector (vector-ref vector i)) pivot)
                           (scan)
                           i)))
                  (j (loop scan ((for j (down-from j)))
                       (if (key< pivot (key-selector (vector-ref vector j)))
                           (scan)
                           j))))
              (if (< i j)
                  (begin (vector-exchange! vector i j)
                         (continue (+ i 1) j))
                  (begin (sort (=> end i))
                         (sort (=> start (+ j 1)))))))))))

(define (vector-exchange! vector i j)
  (let ((vi (vector-ref vector i))
        (vj (vector-ref vector j)))
    (vector-set! vector j vi)
    (vector-set! vector i vj)))



(run-test-suite loop-tests)
