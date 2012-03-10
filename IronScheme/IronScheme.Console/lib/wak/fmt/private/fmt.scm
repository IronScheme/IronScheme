;;;; fmt.scm -- extensible formatting library
;;
;; Copyright (c) 2006-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; (require-extension (srfi 1 6 13 23 69))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string utilities

(define (write-to-string x)
  (call-with-output-string (lambda (p) (write x p))))

(define (display-to-string x)
  (if (string? x)
      x
      (call-with-output-string (lambda (p) (display x p)))))

(define nl-str
  (call-with-output-string newline))

(define (make-space n) (make-string n #\space))
(define (make-nl-space n) (string-append nl-str (make-string n #\space)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list utilities

(define (take* ls n)   ; handles dotted lists and n > length
  (cond ((zero? n) '())
        ((pair? ls) (cons (car ls) (take* (cdr ls) (- n 1))))
        (else '())))

(define (drop* ls n)   ; may return the dot
  (cond ((zero? n) ls)
        ((pair? ls) (drop* (cdr ls) (- n 1)))
        (else ls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; format state representation

;; Use a flexible representation optimized for common cases -
;; frequently accessed values are in fixed vector slots, with a
;; `properties' slot holding an alist for all other values.

(define *default-fmt-state*
  (vector 0 0 10 '() #\space #f 78 #f #f #f #f #f #f))

(define fmt-state? vector?)

(define (new-fmt-state . o)
  (let ((st (if (pair? o) (car o) (current-output-port))))
    (if (vector? st)
        st
        (fmt-set-writer!
         (fmt-set-port! (copy-fmt-state *default-fmt-state*) st)
         fmt-write))))

(define (copy-fmt-state st)
  (let* ((len (vector-length st))
         (res (make-vector len)))
    (do ((i 0 (+ i 1)))
        ((= i len))
      (vector-set! res i (vector-ref st i)))
    (fmt-set-properties! res (map (lambda (x) (cons (car x) (cdr x)))
                                  (fmt-properties res)))
    res))

(define (fmt-row st) (vector-ref st 0))
(define (fmt-col st) (vector-ref st 1))
(define (fmt-radix st) (vector-ref st 2))
(define (fmt-properties st) (vector-ref st 3))
(define (fmt-pad-char st) (vector-ref st 4))
(define (fmt-precision st) (vector-ref st 5))
(define (fmt-width st) (vector-ref st 6))
(define (fmt-writer st) (vector-ref st 7))
(define (fmt-port st) (vector-ref st 8))
(define (fmt-decimal-sep st) (vector-ref st 9))
(define (fmt-decimal-align st) (vector-ref st 10))
(define (fmt-string-width st) (vector-ref st 11))
(define (fmt-ellipses st) (vector-ref st 12))

(define (fmt-set-row! st x) (vector-set! st 0 x) st)
(define (fmt-set-col! st x) (vector-set! st 1 x) st)
(define (fmt-set-radix! st x) (vector-set! st 2 x) st)
(define (fmt-set-properties! st x) (vector-set! st 3 x) st)
(define (fmt-set-pad-char! st x) (vector-set! st 4 x) st)
(define (fmt-set-precision! st x) (vector-set! st 5 x) st)
(define (fmt-set-width! st x) (vector-set! st 6 x) st)
(define (fmt-set-writer! st x) (vector-set! st 7 x) st)
(define (fmt-set-port! st x) (vector-set! st 8 x) st)
(define (fmt-set-decimal-sep! st x) (vector-set! st 9 x) st)
(define (fmt-set-decimal-align! st x) (vector-set! st 10 x) st)
(define (fmt-set-string-width! st x) (vector-set! st 11 x) st)
(define (fmt-set-ellipses! st x) (vector-set! st 12 x) st)

(define (fmt-ref st key . o)
  (case key
    ((row) (fmt-row st))
    ((col) (fmt-col st))
    ((radix) (fmt-radix st))
    ((properties) (fmt-properties st))
    ((writer) (fmt-writer st))
    ((port) (fmt-port st))
    ((precision) (fmt-precision st))
    ((pad-char) (fmt-pad-char st))
    ((width) (fmt-width st))
    ((decimal-sep) (fmt-decimal-sep st))
    ((decimal-align) (fmt-decimal-align st))
    ((string-width) (fmt-string-width st))
    ((ellipses) (fmt-ellipses st))
    (else (cond ((assq key (fmt-properties st)) => cdr)
                ((pair? o) (car o))
                (else #f)))))

(define (fmt-set-property! st key val)
  (cond ((assq key (fmt-properties st))
         => (lambda (cell) (set-cdr! cell val) st))
        (else (fmt-set-properties!
               st
               (cons (cons key val) (fmt-properties st))))))

(define (fmt-set! st key val)
  (case key
    ((row) (fmt-set-row! st val))
    ((col) (fmt-set-col! st val))
    ((radix) (fmt-set-radix! st val))
    ((properties) (fmt-set-properties! st val))
    ((pad-char) (fmt-set-pad-char! st val))
    ((precision) (fmt-set-precision! st val))
    ((writer) (fmt-set-writer! st val))
    ((port) (fmt-set-port! st val))
    ((width) (fmt-set-width! st val))
    ((decimal-sep) (fmt-set-decimal-sep! st val))
    ((decimal-align) (fmt-set-decimal-align! st val))
    ((string-width) (fmt-set-string-width! st val))
    ((ellipses) (fmt-set-ellipses! st val))
    (else (fmt-set-property! st key val))))

(define (fmt-add-properties! st alist)
  (for-each (lambda (x) (fmt-set! st (car x) (cdr x))) alist)
  st)

(define (fmt-let key val . ls)
  (lambda (st)
    (let ((orig-val (fmt-ref st key)))
      (fmt-set! ((apply-cat ls) (fmt-set! st key val)) key orig-val))))

(define (fmt-bind key val . ls)
  (lambda (st) ((apply-cat ls) (fmt-set! st key val))))

(define (fix prec . ls) (fmt-let 'precision prec (apply-cat ls)))
(define (radix rad . ls) (fmt-let 'radix rad (apply-cat ls)))
(define (pad-char ch . ls) (fmt-let 'pad-char ch (apply-cat ls)))
(define (comma-char ch . ls) (fmt-let 'comma-char ch (apply-cat ls)))
(define (decimal-char ch . ls) (fmt-let 'decimal-sep ch (apply-cat ls)))
(define (decimal-align n . ls) (fmt-let 'decimal-align n (apply-cat ls)))
(define (with-width w . ls) (fmt-let 'width w (apply-cat ls)))
(define (ellipses ell . ls) (fmt-let 'ellipses ell (apply-cat ls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the basic interface

(define (fmt-start st initializer proc)
  (cond
    ((or (output-port? st) (fmt-state? st))
     (proc (initializer st))
     (if #f #f))
    ((eq? #t st)
     (proc (initializer (current-output-port)))
     (if #f #f))
    ((eq? #f st)
     (get-output-string
      (fmt-port (proc (initializer (open-output-string))))))
    (else (error "unknown format output" st))))

(define (fmt st . args)
  (fmt-start st new-fmt-state (apply-cat args)))

(define (fmt-update str st)
  (let ((len (string-length str))
        (nli (string-index-right str #\newline))
        (str-width (fmt-string-width st)))
    (if nli
        (let ((row (+ (fmt-row st) 1 (string-count str #\newline 0 nli))))
          (fmt-set-row!
           (fmt-set-col! st (if str-width
                                (str-width str (+ nli 1) len)
                                (- len (+ nli 1))))
           row))
        (fmt-set-col! st (+ (fmt-col st)
                            (if str-width
                                (str-width str 0 len)
                                len))))))

(define (fmt-write str st)
  (display str (fmt-port st))
  (fmt-update str st))

(define (apply-cat procs)
  (lambda (st)
    (let loop ((ls procs) (st st))
      (if (null? ls)
          st
          (loop (cdr ls) ((dsp (car ls)) st))))))

(define (cat . ls) (apply-cat ls))

(define (fmt-null st) st)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; control structures

(define (fmt-if check pass . o)
  (let ((fail (if (pair? o) (car o) (lambda (x) x))))
    (lambda (st) (if (check st) ((dsp pass) st) ((dsp fail) st)))))

(define (fmt-try-fit proc . fail)
  (if (null? fail)
      proc
      (lambda (orig-st)
        (let ((width (fmt-width orig-st))
              (buffer '()))
          (call-with-current-continuation
            (lambda (return)
              (define (output* str st)
                (let lp ((i 0) (col (fmt-col st)))
                  (let ((nli (string-index str #\newline i)))
                    (if nli
                        (if (> (+ (- nli i) col) width)
                            (return ((apply fmt-try-fit fail) orig-st))
                            (lp (+ nli 1) 0))
                        (let* ((len ((or (fmt-string-width st) string-length)
                                     str))
                               (col (+ (- len i) col)))
                          (if (> col width)
                              (return ((apply fmt-try-fit fail) orig-st))
                              (begin
                                (set! buffer (cons str buffer))
                                (fmt-update str st))))))))
              (proc (fmt-set-port! (fmt-set-writer! (copy-fmt-state orig-st)
                                                    output*)
                                   (open-output-string)))
              ((fmt-writer orig-st)
               (string-concatenate-reverse buffer)
               orig-st)))))))

(define (fits-in-width gen width)
  (lambda (st)
    (let ((output (fmt-writer st))
          (port (open-output-string)))
      (call-with-current-continuation
        (lambda (return)
          (define (output* str st)
            (let ((st (fmt-update str st)))
              (if (> (fmt-col st) width)
                  (return #f)
                  (begin
                    (display str port)
                    st))))
          (gen (fmt-set-port! (fmt-set-writer! (copy-fmt-state st) output*)
                              port))
          (get-output-string port))))))

(define (fits-in-columns ls write width)
  (lambda (st)
    (let ((max-w (quotient width 2)))
      (let lp ((ls ls) (res '()) (widest 0))
        (cond
          ((pair? ls)
           (let ((str ((fits-in-width (write (car ls)) max-w) st)))
             (and str
                  (lp (cdr ls)
                      (cons str res)
                      (max ((or (fmt-string-width st) string-length) str)
                           widest)))))
          ((null? ls) (cons widest (reverse res)))
          (else #f))))))

(define (fmt-capture producer consumer)
  (lambda (st)
    (let ((port (open-output-string)))
      (producer (fmt-set-writer! (fmt-set-port! (copy-fmt-state st) port)
                                 fmt-write))
      ((consumer (get-output-string port)) st))))

(define (fmt-to-string producer)
  (fmt-capture producer (lambda (str) (lambda (st) str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; standard formatters

(define (nl st)
  ((fmt-writer st) nl-str st))

;; output a newline iff we're not at the start of a fresh line
(define (fl st)
  (if (zero? (fmt-col st)) st (nl st)))

;; tab to a given tab-stop
(define (tab-to . o)
  (lambda (st)
    (let* ((tab-width (if (pair? o) (car o) 8))
           (rem (modulo (fmt-col st) tab-width)))
      (if (positive? rem)
          ((fmt-writer st)
           (make-string (- tab-width rem) (fmt-pad-char st))
           st)
          st))))

;; move to an explicit column
(define (space-to col)
  (lambda (st)
    (let ((width (- col (fmt-col st))))
      (if (positive? width)
          ((fmt-writer st) (make-string width (fmt-pad-char st)) st)
          st))))

(define (fmt-join fmt ls . o)
  (let ((sep (dsp (if (pair? o) (car o) ""))))
    (lambda (st)
      (if (null? ls)
          st
          (let lp ((ls (cdr ls))
                   (st ((fmt (car ls)) st)))
            (if (null? ls)
                st
                (lp (cdr ls) ((fmt (car ls)) (sep st)))))))))

(define (fmt-join/prefix fmt ls . o)
  (if (null? ls)
      fmt-null
      (let ((sep (dsp (if (pair? o) (car o) ""))))
        (cat sep (fmt-join fmt ls sep)))))
(define (fmt-join/suffix fmt ls . o)
  (if (null? ls)
      fmt-null
      (let ((sep (dsp (if (pair? o) (car o) ""))))
        (cat (fmt-join fmt ls sep) sep))))

(define (fmt-join/last fmt fmt/last ls . o)
  (let ((sep (dsp (if (pair? o) (car o) ""))))
    (lambda (st)
      (cond
        ((null? ls)
         st)
        ((null? (cdr ls))
         ((fmt/last (car ls)) (sep st)))
        (else
         (let lp ((ls (cdr ls))
                  (st ((fmt (car ls)) st)))
           (if (null? (cdr ls))
               ((fmt/last (car ls)) (sep st))
               (lp (cdr ls) ((fmt (car ls)) (sep st))))))))))

(define (fmt-join/dot fmt fmt/dot ls . o)
  (let ((sep (dsp (if (pair? o) (car o) ""))))
    (lambda (st)
      (cond
        ((pair? ls)
         (let lp ((ls (cdr ls))
                  (st ((fmt (car ls)) st)))
           (cond
             ((null? ls) st)
             ((pair? ls) (lp (cdr ls) ((fmt (car ls)) (sep st))))
             (else ((fmt/dot ls) (sep st))))))
        ((null? ls) st)
        (else ((fmt/dot ls) st))))))

(define (fmt-join/range fmt start . o)
  (let-optionals* o ((end #f) (sep ""))
    (lambda (st)
      (let lp ((i (+ start 1)) (st ((fmt start) st)))
        (if (and end (>= i end))
            st
            (lp (+ i 1) ((fmt i) ((dsp sep) st))))))))

(define (pad/both width . ls)
  (fmt-capture
   (apply-cat ls)
   (lambda (str)
     (lambda (st)
       (let ((diff (- width ((or (fmt-string-width st) string-length) str)))
             (output (fmt-writer st)))
         (if (positive? diff)
             (let* ((diff/2 (quotient diff 2))
                    (left (make-string diff/2 (fmt-pad-char st)))
                    (right (if (even? diff)
                               left
                               (make-string (+ 1 diff/2) (fmt-pad-char st)))))
               (output right (output str (output left st))))
             (output str st)))))))

(define (pad width . ls)
  (lambda (st)
    (let* ((col (fmt-col st))
           (padder
            (lambda (st)
              (let ((diff (- width (- (fmt-col st) col))))
                (if (positive? diff)
                    ((fmt-writer st) (make-string diff (fmt-pad-char st)) st)
                    st)))))
      ((cat (apply-cat ls) padder) st))))

(define pad/right pad)

(define (pad/left width . ls)
  (fmt-capture
   (apply-cat ls)
   (lambda (str)
     (lambda (st)
       (let* ((str-width ((or (fmt-string-width st) string-length) str))
              (diff (- width str-width)))
         ((fmt-writer st)
          str
          (if (positive? diff)
              ((fmt-writer st) (make-string diff (fmt-pad-char st)) st)
              st)))))))

(define (trim/buffered width fmt proc)
  (fmt-capture
   fmt
   (lambda (str)
     (lambda (st)
       (let* ((str-width ((or (fmt-string-width st) string-length) str))
              (diff (- str-width width)))
         ((fmt-writer st)
          (if (positive? diff)
              (proc str str-width diff st)
              str)
          st))))))

(define (trim width . ls)
  (lambda (st)
    (let ((ell (fmt-ellipses st)))
      (if ell
          ((trim/buffered
            width
            (apply-cat ls)
            (lambda (str str-width diff st)
              (let* ((ell (if (char? ell) (string ell) ell))
                     (ell-len (string-length ell))
                     (diff (- (+ str-width ell-len) width)))
                (if (negative? diff)
                    ell
                    (string-append
                     (substring/shared str 0 (- (string-length str) diff))
                     ell)))))
           st)
          (let ((output (fmt-writer st))
                (start-col (fmt-col st)))
            (call-with-current-continuation
              (lambda (return)
                (define (output* str st)
                  (let* ((len ((or (fmt-string-width st) string-length) str))
                         (diff (- (+ (- (fmt-col st) start-col) len) width)))
                    (if (positive? diff)
                        (return
                         (fmt-set-writer!
                          (output (substring/shared str 0 (- len diff)) st)
                          output))
                        (output str st))))
                ((fmt-let 'writer output* (apply-cat ls)) st))))))))

(define (trim/length width . ls)
  (lambda (st)
    (call-with-current-continuation
      (lambda (return)
        (let ((output (fmt-writer st))
              (sum 0))
          (define (output* str st)
            (let ((len (string-length str)))
              (set! sum (+ sum len))
              (if (> sum width)
                  (return
                   (fmt-set-writer!
                    (output (substring/shared str 0 (- len (- sum width))) st)
                    output))
                  (output str st))))
          ((fmt-let 'writer output* (apply-cat ls)) st))))))

(define (trim/left width . ls)
  (trim/buffered
   width
   (apply-cat ls)
   (lambda (str str-width diff st)
     (let ((ell (fmt-ellipses st)))
       (if ell
           (let* ((ell (if (char? ell) (string ell) ell))
                  (ell-len (string-length ell))
                  (diff (- (+ str-width ell-len) width)))
             (if (negative? diff)
                 ell
                 (string-append ell (substring/shared str diff))))
           (substring/shared str diff))))))

(define (trim/both width . ls)
  (trim/buffered
   width
   (apply-cat ls)
   (lambda (str str-width diff st)
     (let ((ell (fmt-ellipses st)))
       (if ell
           (let* ((ell (if (char? ell) (string ell) ell))
                  (ell-len (string-length ell))
                  (diff (- (+ str-width ell-len ell-len) width))
                  (left (quotient diff 2))
                  (right (- (string-length str) (quotient (+ diff 1) 2))))
             (if (negative? diff)
                 ell
                 (string-append ell (substring/shared str left right) ell)))
           (substring/shared str
                             (quotient (+ diff 1) 2)
                             (- (string-length str) (quotient diff 2))))))))

(define (fit width . ls)
  (pad width (trim width (apply-cat ls))))
(define (fit/left width . ls)
  (pad/left width (trim/left width (apply-cat ls))))
(define (fit/both width . ls)
  (pad/both width (trim/both width (apply-cat ls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; String-map formatters

(define (make-string-fmt-transformer proc)
  (lambda ls
    (lambda (st)
      (let ((base-writer (fmt-writer st)))
        ((fmt-let
          'writer (lambda (str st) (base-writer (proc str) st))
          (apply-cat ls))
         st)))))

(define upcase (make-string-fmt-transformer string-upcase))
(define downcase (make-string-fmt-transformer string-downcase))
(define titlecase (make-string-fmt-transformer string-titlecase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numeric formatting

(define *min-e* -1024)
(define *bot-f* (expt 2 52))
;;(define *top-f* (* 2 *bot-f*))

(define (integer-log a base)
  (if (zero? a)
      0
      (inexact->exact (ceiling (/ (log (+ a 1)) (log base))))))
(define (integer-length* a)
  (if (negative? a)
      (integer-log (- 1 a) 2)
      (integer-log a 2)))

(define invlog2of
  (let ((table (make-vector 37))
        (log2 (log 2)))
    (do ((b 2 (+ b 1)))
        ((= b 37))
      (vector-set! table b (/ log2 (log b))))
    (lambda (b)
      (if (<= 2 b 36)
          (vector-ref table b)
          (/ log2 (log b))))))

(define fast-expt
  (let ((table (make-vector 326)))
    (do ((k 0 (+ k 1)) (v 1 (* v 10)))
        ((= k 326))
      (vector-set! table k v))
    (lambda (b k)
      (if (and (= b 10) (<= 0 k 326))
          (vector-ref table (inexact->exact (truncate k)))
          (expt b k)))))

(define (mirror-of c)
  (case c ((#\() #\)) ((#\[) #\]) ((#\{) #\}) ((#\<) #\>) (else c)))

(define default-digits
  (list->vector (string->list "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))

;; kanji (10 included for base 11 ;)
;; (vector "０" "一" "二" "三" "四" "五" "六" "七" "八" "九" "十")

;; old style kanji:
;; (vector "零" "壱" "弐" "参" "肆" "伍" "陸" "柒" "捌" "玖" "拾")

;; General algorithm based on "Printing Floating-Point Numbers Quickly
;; and Accurately" by Burger and Dybvig (FP-Printing-PLDI96.pdf).  The
;; code below will be hard to read out of that context until it's
;; cleaned up.

(define (num->string n st . opt)
  (call-with-output-string
    (lambda (port)
      (let-optionals* opt
          ((base (fmt-radix st))
           (digits (fmt-precision st))
           (sign? #f)
           (commify? #f)
           (comma-sep (and commify? (fmt-ref st 'comma-char #\,)))
           (decimal-sep (or (fmt-decimal-sep st)
                            (if (eqv? comma-sep #\.) #\, #\.)))
           (comma-rule (if (eq? commify? #t) 3 commify?))
           (align (fmt-decimal-align st))
           (digit-vec default-digits)
           (stack '()))

        (define (write-digit d)
          (display (vector-ref digit-vec (inexact->exact (truncate d))) port))

        ;; This is ugly because we need to keep a list of all output
        ;; of the form x9999... in case we get to the end of the
        ;; precision and need to round up.  Alas, if it weren't for
        ;; decimals and commas, we could just keep track of the last
        ;; non-9 digit and the number of nines seen, without any need
        ;; for a heap-allocated stack.
        (define (write-digit-list ls)
          (for-each
           (lambda (x) (if (number? x) (write-digit x) (display x port)))
           ls))

        (define (flush)
          (write-digit-list (reverse stack))
          (set! stack '()))

        (define (flush/rounded)
          (let lp ((ls stack) (res '()))
            (cond
             ((null? ls)
              (write-digit-list (cons #\1 res)))
             ((not (number? (car ls)))
              (lp (cdr ls) (cons (car ls) res)))
             ((= (car ls) (- base 1))
              (lp (cdr ls) (cons #\0 res)))
             (else
              (write-digit-list
               (append (reverse (cdr ls)) (cons (+ 1 (car ls)) res))))))
          (set! stack '()))

        (define (output digit)
          (if (and (number? digit) (< digit (- base 1)))
              (flush))
          (set! stack (cons digit stack)))

        (define (write-prefix prefix align k)
          (if align
              (let* ((prefix (cond ((string? prefix) prefix)
                                   ((char? prefix) (string prefix))
                                   (else "")))
                     (diff (- align
                              (+ (if (zero? k) 1 k) (string-length prefix))
                              1)))
                (if (positive? diff)
                    (display (make-string diff (fmt-pad-char st)) port))
                (display prefix port))
              (if prefix (display prefix port)))) 

        (define (write-real n prefix align)

          (let* ((m+e (mantissa+exponent (exact->inexact n)))
                 (f (car m+e))
                 (e (cadr m+e))
                 (inv-base (invlog2of base))
                 (round? (even? f))
                 (smaller (if round? <= <))
                 (bigger (if round? >= >)))

            (define (pad d i) ;; just pad 0's, not #'s
              (write-digit d)
              (let lp ((i (- i 1)))
                (cond
                 ((>= i 0)
                  (if (and commify?
                           (if digits
                               (and (> i digits)
                                    (zero? (modulo (- i (- digits 1))
                                                   comma-rule)))
                               (and (positive? i)
                                    (zero? (modulo i comma-rule)))))
                      (display comma-sep port))
                  (if (= i (- digits 1))
                      (display decimal-sep port))
                  (write-digit 0)
                  (lp (- i 1))))))

            (define (pad-all d i)
              (cond
               ((>= d base)
                (flush/rounded))
               (else
                (flush)
                (write-digit d)))
              (let lp ((i (- i 1)))
                (cond
                 ((> i 0)
                  (if (and commify? (zero? (modulo i comma-rule)))
                      (display comma-sep port))
                  (write-digit 0)
                  (lp (- i 1)))
                 ((and (= i 0) (inexact? n))
                  (display decimal-sep port)
                  (write-digit 0)))))

            ;;(define (pad-sci d i k)
            ;;  (cond
            ;;   ((>= d base)
            ;;    (flush/rounded))
            ;;   (else
            ;;    (flush)
            ;;    (write-digit d)))
            ;;  (write-char #\e port)
            ;;  (cond
            ;;   ((positive? k)
            ;;    (write-char #\+ port)
            ;;    (write (- k 1) port))
            ;;   (else
            ;;    (write k port))))

            (define (scale r s m+ m- k f e)
              (let ((est (inexact->exact
                          (ceiling (- (* (+ e (integer-length* f) -1)
                                         (invlog2of base))
                                      1.0e-10)))))
                (if (not (negative? est))
                    (fixup r (* s (fast-expt base est)) m+ m- est)
                    (let ((skale (fast-expt base (- est))))
                      (fixup (* r skale) s (* m+ skale) (* m- skale) est)))))

            (define (fixup r s m+ m- k)
              (if (and (bigger (+ r m+) s)) ;; (or digits (>= k -4))
                  (lead r s m+ m- (+ k 1))
                  (lead (* r base) s (* m+ base) (* m- base) k)))

            (define (lead r s m+ m- k)
              (write-prefix prefix align k)
              (cond
               ((and (not digits) (or (> k 14) (< k -4)))
                (write n port))      ; XXXX native write for sci
               ;;((and (not digits) (> k 14))
               ;; (generate-sci r s m+ m- k))
               ;;((and (not digits) (< k -4))
               ;; (if (>= (/ r s) base)
               ;;     (generate-sci (/ r base) s (/ m+ base) (/ m- base) k)
               ;;     (generate-sci r s m+ m- k)))
               (else
                (cond
                 ((and (not digits)
                       (or (negative? k)
                           (and (zero? k) (not (integer? n)))))
                  (write-digit 0)
                  (display decimal-sep port)
                  (let lp ((i 0))
                    (cond ((> i k)
                           (write-digit 0)
                           (lp (- i 1)))))))
                (if digits
                    (generate-fixed r s m+ m- k)
                    (generate-all r s m+ m- k)))))

            (define (generate-all r s m+ m- k)
              (let gen ((r r) (m+ m+) (m- m-) (i k))
                (cond ((= i k))
                      ((zero? i)
                       (output decimal-sep))
                      ((and commify?
                            (positive? i)
                            (zero? (modulo i comma-rule)))
                       (output comma-sep)))
                (let ((d (quotient r s))
                      (r (remainder r s)))
                  (if (not (smaller r m-))
                      (cond
                       ((not (bigger (+ r m+) s))
                        (output d)
                        (gen (* r base) (* m+ base) (* m- base) (- i 1)))
                       (else
                        (pad-all (+ d 1) i)))
                      (if (not (bigger (+ r m+) s))
                          (pad-all d i)
                          (pad-all (if (< (* r 2) s) d (+ d 1)) i))))))

            (define (generate-fixed r s m+ m- k)
              (if (<= k 0) 
                  (set! stack (append (make-list (min (- k) digits) 0)
                                      (list decimal-sep 0))))
              (let ((i0 (- (+ k digits) 1)))
                (let gen ((r r) (m+ m+) (m- m-) (i i0))
                  (cond ((= i i0))
                        ((= i (- digits 1))
                         (output decimal-sep))
                        ((and commify?
                              (> i digits)
                              (zero? (modulo (- i (- digits 1))
                                             comma-rule)))
                         (output comma-sep)))
                  (let ((d (quotient r s))
                        (r (remainder r s)))
                    (cond
                     ((< i 0)
                      (let ((d2 (* 2 (if (>= (* r 2) s) (+ d 1) d))))
                        (if (and (not (> (- k) digits))
                                 (or (> d2 base)
                                     (and (= d2 base)
                                          (pair? stack)
                                          (number? (car stack))
                                          (odd? (car stack)))))
                            (flush/rounded)
                            (flush))))
                     ((smaller r m-)
                      (cond
                       ((>= d base)
                        (flush/rounded)
                        (pad 0 i))
                       (else
                        (flush)
                        (if (bigger (+ r m+) s)
                            (pad (if (< (* r 2) s) d (+ d 1)) i)
                            (pad d i)))))
                     ((bigger (+ r m+) s)
                      (cond
                       ((>= d (- base 1))
                        (flush/rounded)
                        (pad 0 i))
                       (else
                        (flush)
                        (pad (+ d 1) i))))
                     (else
                      (output d)
                      (gen (* r base) (* m+ base) (* m- base) (- i 1))))))))

            ;;(define (generate-sci r s m+ m- k)
            ;;  (let gen ((r r) (m+ m+) (m- m-) (i k))
            ;;    (cond ((= i (- k 1)) (display decimal-sep port)))
            ;;    (let ((d (quotient r s))
            ;;          (r (remainder r s)))
            ;;      (if (not (smaller r m-))
            ;;          (cond
            ;;           ((not (bigger (+ r m+) s))
            ;;            (output d)
            ;;            (gen (* r base) (* m+ base) (* m- base) (- i 1)))
            ;;           (else (pad-sci (+ d 1) i k)))
            ;;          (if (not (bigger (+ r m+) s))
            ;;              (pad-sci d i k)
            ;;              (pad-sci (if (< (* r 2) s) d (+ d 1)) i k))))))

            (cond
             ((negative? e)
              (if (or (= e *min-e*) (not (= f *bot-f*)))
                  (scale (* f 2) (* (expt 2.0 (- e)) 2) 1 1 0 f e)
                  (scale (* f 2 2) (* (expt 2.0 (- 1 e)) 2) 2 1 0 f e)))
             (else
              (if (= f *bot-f*)
                  (let ((be (expt 2 e)))
                    (scale (* f be 2) 2.0 be be 0 f e))
                  (let* ((be (expt 2 e)) (be1 (* be 2)))
                    (scale (* f be1 2) (* 2.0 2) be1 be 0 f e)))))))

        (define (write-fixed-rational p prefix align)
          (define (get-scale q) (expt base (- (integer-log q base) 1)))
          (let ((n (numerator p))
                (d (denominator p))
                (k (integer-log p base)))
            (write-prefix prefix align k)
            (let lp ((n n)
                     (i (- k)))
              (cond
               ((< i digits)
                (if (zero? i) (output decimal-sep))
                (let ((q (quotient n d)))
                  (cond
                   ((>= q base)
                    (let* ((scale (get-scale q))
                           (digit (quotient q scale))
                           (n2 (- n (* d digit scale))))
                      (output digit)
                      (lp n2 (+ i 1))))
                   (else
                    (output q)
                    (lp (* (remainder n d) base) (+ i 1))))))
               (else
                (let* ((q (quotient n d))
                       (digit
                        (* 2 (if (>= q base) (quotient q (get-scale q)) q))))
                  (if (or (> digit base)
                          (and (= digit base)
                               (let ((prev (find integer? stack)))
                                 (and prev (odd? prev)))))
                      (flush/rounded)
                      (flush))))))))

        (define (wrap-sign n sign? align writer)
          (cond
           ((negative? n)
            (cond
             ((char? sign?)
              (writer (abs n) sign? align)
              (display (mirror-of sign?) port))
             (else
              (writer (abs n) #\- align))))
           (else
            (cond
             ((and sign? (not (char? sign?)))
              (writer n #\+ align))
             (else
              (writer n #f align))))))

        (let ((imag (imag-part n)))
          (cond
           ((and base (not (and (integer? base) (<= 2 base 36))))
            (error "invalid base for numeric formatting" base))
           ((zero? imag)
            (cond
             ((and (exact? n) (not (integer? n)))
              (cond
               (digits
                (wrap-sign n sign? align write-fixed-rational))
               (else
                (wrap-sign (numerator n) sign? #f write-real)
                (write-char #\/ port)
                (wrap-sign (denominator n) #f #f write-real))))
             (else
              (wrap-sign n sign? align write-real))))
           (else (wrap-sign (real-part n) sign? #f write-real)
                 (wrap-sign imag #t #f write-real)
                 (write-char #\i port))))))))

(define (num n . opt)
  (lambda (st) ((fmt-writer st) (apply num->string n st opt) st)))

(define (num/comma n . o)
  (lambda (st)
    (let-optionals* o
        ((base (fmt-radix st))
         (digits (fmt-precision st))
         (sign? #f)
         (comma-rule 3)
         (comma-sep (fmt-ref st 'comma-char #\,))
         (decimal-sep (or (fmt-decimal-sep st)
                          (if (eqv? comma-sep #\.) #\, #\.))))
      ((num n base digits sign? comma-rule comma-sep decimal-sep) st))))

;; SI suffix formatting, as used in --human-readable options to some
;; GNU commands (such as ls).  See
;;
;;   http://www.bipm.org/en/si/si_brochure/chapter3/prefixes.html
;;   http://physics.nist.gov/cuu/Units/binary.html
;;
;; Note: lowercase "k" for base 10, uppercase "K" for base 2

(define num/si
  (let* ((names10 '#("" "k" "M" "G" "T" "E" "P" "Z" "Y"))
         (names2 (list->vector
                  (cons ""
                        (cons "Ki" (map (lambda (s) (string-append s "i"))
                                        (cddr (vector->list names10))))))))
    (lambda (n . o)
      (let-optionals* o ((base 1024)
                         (suffix "")
                         (names (if (= base 1024) names2 names10)))
        (let* ((k (min (inexact->exact (floor (/ (log n) (log base))))
                       (vector-length names)))
               (n2 (/ (round (* (/ n (expt base k)) 10)) 10)))
          (cat (if (integer? n2)
                   (number->string (inexact->exact n2))
                   (exact->inexact n2))
               (vector-ref names k)
               (if (zero? k) "" suffix)))))))

(define roman-numerals
  '((1000 . #\M) (500 . #\D) (100 . #\C)
    (50 . #\L) (10 . #\X) (5 . #\V) (1 . #\I)))

(define (num/old-roman num)
  (lambda (st)
    (let lp ((num num) (res '()))
      (if (positive? num)
          (let ((ch (find (lambda (x) (>= num (car x))) roman-numerals)))
            (lp (- num (car ch)) (cons (cdr ch) res)))
          (fmt-write (reverse-list->string res) st)))))

(define (num/roman num)
  (lambda (st)
    (let lp1 ((num num) (res '()))
      (if (positive? num)
          (let lp2 ((ls roman-numerals))
               (let* ((big (car ls))
                      (big-n (car big)))
                 (cond
                  ((>= num big-n)
                   (lp1 (- num big-n) (cons (cdr big) res)))
                  ((and (> (* 2 num) big-n)
                        (find (lambda (c)
                                (let ((x (car c)))
                                  (<= (+ x 1) (- big-n x) num)))
                              ls))
                   => (lambda (c)
                        (lp1 (- num (- big-n (car c)))
                             (cons (cdr big) (cons (cdr c) res)))))
                  (else
                   (lp2 (cdr ls))))))
          (fmt-write (reverse-list->string res) st)))))

;; Force a number into a fixed width, print as #'s if doesn't fit.
;; Needs to be wrapped in a PAD if you want to expand to the width.

(define (num/fit width n . args)
  (fmt-capture
   (apply num n args)
   (lambda (str)
     (lambda (st)
       (if (> (string-length str) width)
           (let ((prec (if (and (pair? args) (pair? (cdr args)))
                           (cadr args)
                           (fmt-precision st))))
             (if prec
                 (let* ((decimal-sep
                         (or (fmt-ref st 'decimal-sep)
                             (if (eqv? #\. (fmt-ref st 'comma-sep)) #\, #\.)))
                        (diff (- width (+ prec
                                          (if (char? decimal-sep)
                                              1
                                              (string-length decimal-sep))))))
                   ((cat (if (positive? diff) (make-string diff #\#) "")
                         decimal-sep (make-string prec #\#))
                    st))
                 ((fmt-writer st) (make-string width #\#) st)))
           ((fmt-writer st) str st))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shared structure utilities

(define (eq?-table-ref tab x) (hash-table-ref/default tab x #f))
(define (eq?-table-set! tab x v) (hash-table-set! tab x v))

;; XXXX extend for records and other container data types
(define (make-shared-ref-table obj)
  (let ((tab (make-eq?-table))
        (res (make-eq?-table))
        (index 0))
    (let walk ((obj obj))
      (cond
        ((eq?-table-ref tab obj)
         => (lambda (i) (eq?-table-set! tab obj (+ i 1))))
        ((not (or (symbol? obj) (number? obj) (char? obj)
                  (boolean? obj) (null? obj) (eof-object? obj)))
         (eq?-table-set! tab obj 1)
         (cond
           ((pair? obj)
            (walk (car obj))
            (walk (cdr obj)))
           ((vector? obj)
            (let ((len (vector-length obj)))
              (do ((i 0 (+ i 1))) ((>= i len))
                (walk (vector-ref obj i)))))))))
    (hash-table-walk
     tab
     (lambda (obj count)
       (if (> count 1)
           (begin
             (eq?-table-set! res obj (cons index #f))
             (set! index (+ index 1))))))
    res))

(define (gen-shared-ref i suffix)
  (string-append "#" (number->string i) suffix))

(define (maybe-gen-shared-ref st cell shares)
  (cond
    ((pair? cell)
     (set-car! cell (cdr shares))
     (set-cdr! cell #t)
     (set-cdr! shares (+ (cdr shares) 1))
     ((fmt-writer st) (gen-shared-ref (car cell) "=") st))
    (else st)))

(define (call-with-shared-ref obj st shares proc)
  (let ((cell (eq?-table-ref (car shares) obj)))
    (if (and (pair? cell) (cdr cell))
        ((fmt-writer st) (gen-shared-ref (car cell) "#") st)
        (proc (maybe-gen-shared-ref st cell shares)))))

(define (call-with-shared-ref/cdr obj st shares proc sep)
  (let ((cell (eq?-table-ref (car shares) obj))
        (output (fmt-writer st)))
    (cond
      ((and (pair? cell) (cdr cell))
       (output (gen-shared-ref (car cell) "#") (output ". " (sep st))))
      ((pair? cell)
       (let ((st (maybe-gen-shared-ref (output ". " (sep st)) cell shares)))
         (output ")" (proc (output "(" st)))))
      (else
       (proc (sep st))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sexp formatters

(define (slashified str . o)
  (let-optionals* o ((quot #\") (esc #\\) (rename (lambda (x) #f)))
    (lambda (st)
      (let* ((len (string-length str))
             (output (fmt-writer st))
             (quot-str (string quot))
             (esc-str (if (char? esc) (string esc) (or esc quot-str))))
        (let lp ((i 0) (j 0) (st st))
          (define (collect)
            (if (= i j) st (output (substring/shared str i j) st)))
          (if (>= j len)
              (collect)
              (let ((c (string-ref str j)))
                (cond
                  ((or (eqv? c quot) (eqv? c esc))
                   (lp j (+ j 1) (output esc-str (collect))))
                  ((rename c)
                   => (lambda (c2)
                        (lp (+ j 1)
                            (+ j 1)
                            (output c2 (output esc-str (collect))))))
                  (else
                   (lp i (+ j 1) st))))))))))

;; Only slashify if there are special characters, in which case also
;; wrap in quotes.  For writing symbols in |...| escapes, or CSV
;; fields, etc.  The predicate indicates which characters cause
;; slashification - this is in addition to automatic slashifying when
;; either the quote or escape char is present.

(define (maybe-slashified str pred . o)
  (let-optionals* o ((quot #\") (esc #\\) (rename (lambda (x) #f)))
    (define (esc? c) (or (eqv? c quot) (eqv? c esc) (rename c) (pred c)))
    (if (string-index str esc?)
        (cat quot (slashified str quot esc rename) quot)
        (dsp str))))

(define (fmt-write-string str)
  (define (rename c)
    (case c
      ((#\newline) "n")
      (else #f)))
  (slashified str #\" #\\ rename))

(define (dsp obj)
  (cond
    ((procedure? obj) obj)
    ((string? obj) (lambda (st) ((fmt-writer st) obj st)))
    ((char? obj) (dsp (string obj)))
    (else (wrt obj))))

(define (write-with-shares obj shares)
  (lambda (st)
    (let* ((output (fmt-writer st))
           (wr-num
            (cond ((and (= 10 (fmt-radix st))
                        (not (fmt-precision st))
                        (not (fmt-decimal-align st)))
                   (lambda (n st) (output (number->string n) st)))
                  ((assv (fmt-radix st)
                         '((16 . "#x") (10 . "") (8 . "#o") (2 . "#b")))
                   => (lambda (cell)
                        (let ((prefix (cdr cell)))
                          (lambda (n st) ((num n) (output prefix st))))))
                  (else (lambda (n st) (output (number->string n) st))))))
      (let wr ((obj obj) (st st))
        (call-with-shared-ref obj st shares
          (lambda (st)
            (cond
              ((pair? obj)
               (output
                ")"
                (let lp ((ls obj)
                         (st (output "(" st)))
                  (let ((st (wr (car ls) st))
                        (rest (cdr ls)))
                    (cond
                      ((null? rest) st)
                      ((pair? rest)
                       (call-with-shared-ref/cdr rest st shares
                         (lambda (st) (lp rest st))
                         (dsp " ")))
                      (else (wr rest (output " . " st))))))))
              ((vector? obj)
               (let ((len (vector-length obj)))
                 (if (zero? len)
                     (output "#()" st)
                     (let lp ((i 1)
                              (st
                               (wr (vector-ref obj 0)
                                   (output "#(" st))))
                       (if (>= i len)
                           (output ")" st)
                           (lp (+ i 1)
                               (wr (vector-ref obj i)
                                   (output " " st))))))))
              ((string? obj)
               (output "\"" ((fmt-write-string obj) (output "\"" st))))
              ((number? obj)
               (wr-num obj st))
              ((boolean? obj)
               (output (if obj "#t" "#f") st))
              (else
               (output (write-to-string obj) st)))))))))

(define (wrt obj)
  (write-with-shares obj (cons (make-shared-ref-table obj) 0)))

;; the only expensive part, in both time and memory, of handling
;; shared structures when writing is building the initial table, so
;; for the efficient version we just skip that

(define (wrt/unshared obj)
  (write-with-shares obj (cons (make-eq?-table) 0)))

