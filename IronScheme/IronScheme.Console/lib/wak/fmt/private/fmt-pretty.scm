;;;; fmt-pretty.scm -- pretty printing format combinator
;;
;; Copyright (c) 2006-2007 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; additional settings

(define (fmt-shares st) (fmt-ref st 'shares))
(define (fmt-set-shares! st x) (fmt-set! st 'shares x))
(define (fmt-copy-shares st)
  (fmt-set-shares! (copy-fmt-state st) (copy-shares (fmt-shares st))))

(define (copy-shares shares)
  (let ((tab (make-eq?-table)))
    (hash-table-walk
     (car shares)
     (lambda (obj x) (eq?-table-set! tab obj (cons (car x) (cdr x)))))
    (cons tab (cdr shares))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities

(define (fmt-shared-write obj proc)
  (lambda (st)
    (let* ((shares (fmt-shares st))
           (cell (and shares (eq?-table-ref (car shares) obj))))
      (if (pair? cell)
          (cond
            ((cdr cell)
             ((fmt-writer st) (gen-shared-ref (car cell) "#") st))
            (else
             (set-car! cell (cdr shares))
             (set-cdr! cell #t)
             (set-cdr! shares (+ (cdr shares) 1))
             (proc ((fmt-writer st) (gen-shared-ref (car cell) "=") st))))
          (proc st)))))

(define (fmt-join/shares fmt ls . o)
  (let ((sep (dsp (if (pair? o) (car o) " "))))
    (lambda (st)
      (if (null? ls)
          st
          (let* ((shares (fmt-shares st))
                 (tab (car shares))
                 (output (fmt-writer st)))
            (let lp ((ls ls) (st st))
              (let ((st ((fmt (car ls)) st))
                    (rest (cdr ls)))
                (cond
                 ((null? rest) st)
                 ((pair? rest)
                  (call-with-shared-ref/cdr rest st shares
                      (lambda (st) (lp rest st))
                    sep))
                 (else ((fmt rest) (output ". " (sep st))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pretty printing

(define (non-app? x)
  (if (pair? x)
      (or (not (or (null? (cdr x)) (pair? (cdr x))))
          (non-app? (car x)))
      (not (symbol? x))))

(define syntax-abbrevs
  '((quote . "'") (quasiquote . "`") (unquote . ",") (unquote-splicing . ",@")
    ))

(define (pp-let ls)
  (if (and (pair? (cdr ls)) (symbol? (cadr ls)))
      (pp-with-indent 2 ls)
      (pp-with-indent 1 ls)))

(define indent-rules
  `((lambda . 1) (define . 1)
    (let . ,pp-let) (loop . ,pp-let)
    (let* . 1) (letrec . 1) (letrec* . 1) (and-let* . 1) (let1 . 2)
    (let-values . 1) (let*-values . 1) (receive . 2) (parameterize . 1)
    (let-syntax . 1) (letrec-syntax . 1) (syntax-rules . 1) (syntax-case . 2)
    (match . 1) (match-let . 1) (match-let* . 1)
    (if . 3) (when . 1) (unless . 1) (case . 1) (while . 1) (until . 1)
    (do . 2) (dotimes . 1) (dolist . 1) (test . 1)
    (condition-case . 1) (guard . 1) (rec . 1)
    (call-with-current-continuation . 0)
    ))

(define indent-prefix-rules
  `(("with-" . -1) ("call-with-" . -1) ("define-" . 1))
  )

(define indent-suffix-rules
  `(("-case" . 1))
  )

(define (pp-indentation form)
  (let ((indent
         (cond
          ((assq (car form) indent-rules) => cdr)
          ((and (symbol? (car form))
                (let ((str (symbol->string (car form))))
                  (or (find (lambda (rx) (string-prefix? (car rx) str))
                            indent-prefix-rules)
                      (find (lambda (rx) (string-suffix? (car rx) str))
                            indent-suffix-rules))))
           => cdr)
          (else #f))))
    (if (and (number? indent) (negative? indent))
        (max 0 (- (+ (length+ form) indent) 1))
        indent)))

(define (pp-with-indent indent-rule ls)
  (lambda (st)
    (let* ((col1 (fmt-col st))
           (st ((cat "(" (pp-object (car ls))) st))
           (col2 (fmt-col st))
           (fixed (take* (cdr ls) (or indent-rule 1)))
           (tail (drop* (cdr ls) (or indent-rule 1)))
           (st2 (fmt-copy-shares st))
           (first-line
            ((fmt-to-string (cat " " (fmt-join/shares pp-flat fixed " "))) st2))
           (default
             (let ((sep (make-nl-space (+ col1 1))))
               (cat sep (fmt-join/shares pp-object (cdr ls) sep) ")"))))
      (cond
       ((< (+ col2 (string-length first-line)) (fmt-width st2))
        ;; fixed values on first line
        (let ((sep (make-nl-space
                    (if indent-rule (+ col1 2) (+ col2 1)))))
          ((cat first-line
                (cond
                 ((not (or (null? tail) (pair? tail)))
                  (cat ". " (pp-object tail)))
                 ((> (length+ (cdr ls)) (or indent-rule 1))
                  (cat sep (fmt-join/shares pp-object tail sep)))
                 (else
                  fmt-null))
                ")")
           st2)))
       (indent-rule ;;(and indent-rule (not (pair? (car ls))))
        ;; fixed values lined up, body indented two spaces
        ((fmt-try-fit
          (lambda (st)
            ((cat
              " "
              (fmt-join/shares pp-object fixed (make-nl-space (+ col2 1)))
              (if (pair? tail)
                  (let ((sep (make-nl-space (+ col1 2))))
                    (cat sep (fmt-join/shares pp-object tail sep)))
                  "")
              ")")
             (fmt-copy-shares st)))
          default)
         st))
       (else
        ;; all on separate lines
        (default st))))))

(define (pp-app ls)
  (let ((indent-rule (pp-indentation ls)))
    (if (procedure? indent-rule)
        (indent-rule ls)
        (pp-with-indent indent-rule ls))))

;; the elements may be shared, just checking the top level list
;; structure
(define (proper-non-shared-list? ls shares)
  (let ((tab (car shares)))
    (let lp ((ls ls))
      (or (null? ls)
          (and (pair? ls)
               (not (eq?-table-ref tab ls))
               (lp (cdr ls)))))))

(define (pp-flat x)
  (cond
    ((pair? x)
     (fmt-shared-write
      x
      (cond
        ((and (pair? (cdr x)) (null? (cddr x))
              (assq (car x) syntax-abbrevs))
         => (lambda (abbrev)
              (cat (cdr abbrev) (pp-flat (cadr x)))))
        (else
         (cat "(" (fmt-join/shares pp-flat x " ") ")")))))
    ((vector? x)
     (fmt-shared-write
      x
      (cat "#(" (fmt-join/shares pp-flat (vector->list x) " ") ")")))
    (else
     (lambda (st) ((write-with-shares x (fmt-shares st)) st)))))

(define (pp-pair ls)
  (fmt-shared-write
   ls
   (cond
    ;; one element list, no lines to break
    ((null? (cdr ls))
     (cat "(" (pp-object (car ls)) ")"))
    ;; quote or other abbrev
    ((and (pair? (cdr ls)) (null? (cddr ls))
          (assq (car ls) syntax-abbrevs))
     => (lambda (abbrev)
          (cat (cdr abbrev) (pp-object (cadr ls)))))
    (else
     (fmt-try-fit
      (lambda (st) ((pp-flat ls) (fmt-copy-shares st)))
      (lambda (st)
        (if (and (non-app? ls)
                 (proper-non-shared-list? ls (fmt-shares st)))
            ((pp-data-list ls) st)
            ((pp-app ls) st))))))))

(define (pp-data-list ls)
  (lambda (st)
    (let* ((output (fmt-writer st))
           (st (output "(" st))
           (col (fmt-col st))
           (width (- (fmt-width st) col))
           (st2 (fmt-copy-shares st)))
      (cond
        ((and (pair? (cdr ls)) (pair? (cddr ls)) (pair? (cdddr ls))
              ((fits-in-columns ls pp-flat width) st2))
         => (lambda (ls)
              ;; at least four elements which can be broken into columns
              (let* ((prefix (make-nl-space (+ col 1)))
                     (widest (+ 1 (car ls)))
                     (columns (quotient width widest))) ; always >= 2
                (let lp ((ls (cdr ls)) (st st2) (i 1))
                  (cond
                    ((null? ls)
                     (output ")" st))
                    ((null? (cdr ls))
                     (output ")" (output (car ls) st)))
                    (else
                     (let ((st (output (car ls) st)))
                       (if (>= i columns)
                           (lp (cdr ls) (output prefix st) 1)
                           (let* ((pad (- widest (string-length (car ls))))
                                  (st (output (make-space pad) st)))
                             (lp (cdr ls) st (+ i 1)))))))))))
        (else
         ;; no room, print one per line
         ((cat (fmt-join pp-object ls (make-nl-space col)) ")") st))))))

(define (pp-vector vec)
  (fmt-shared-write vec (cat "#" (pp-data-list (vector->list vec)))))

(define (pp-object obj)
  (cond
    ((pair? obj) (pp-pair obj))
    ((vector? obj) (pp-vector obj))
    (else (lambda (st) ((write-with-shares obj (fmt-shares st)) st)))))

(define (pretty obj)
  (fmt-bind 'shares (cons (make-shared-ref-table obj) 0)
            (cat (pp-object obj) fl)))

(define (pretty/unshared obj)
  (fmt-bind 'shares (cons (make-eq?-table) 0) (cat (pp-object obj) fl)))

