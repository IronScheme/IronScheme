#| License
Copyright (c) 2007-2014 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

;;"genwrite.scm" generic write used by pretty-print and truncated-print.
;; Copyright (c) 1991, Marc Feeley
;; Author: Marc Feeley (feeley@iro.umontreal.ca)
;; Distribution restrictions: none

(library (ironscheme pretty-print)
  (export 
    pretty-print 
    pretty-width
    pretty-gensyms)
  (import 
    (rename 
      (except (ironscheme) 
        generic-write
        pretty-gensyms
        pretty-print 
        pretty-width) 
      (symbol->string rnrs:symbol->string))
    (rnrs mutable-strings))

(define genwrite:newline-str (make-string 1 #\newline))

(define (symbol->string s)
  (rnrs:symbol->string
    (if (pretty-gensyms)
        (ungensym s)
        s)))

(define (read-macro-body l)
  (cadr l))

(define (read-macro-prefix l)
  (let ((head (car l)) (tail (cdr l)))
    (case head
      ((syntax)            "#'")
      ((quasisyntax)       "#`")
      ((unsyntax)          "#,")
      ((unsyntax-splicing) "#,@")      
      ((quote)            "'")
      ((quasiquote)       "`")
      ((unquote)          ",")
      ((unquote-splicing) ",@"))))
        
(define (length1? l) (and (pair? l) (null? (cdr l))))        

(define (read-macro? l)
  (let ((head (car l)) (tail (cdr l)))
    (case head
      ((quote quasiquote unquote unquote-splicing 
        syntax quasisyntax unsyntax unsyntax-splicing) 
        (length1? tail))
      (else #f))))

(define (generic-write obj display? width output)
  (define (out str col)
    (and col (output str) (fx+ col (string-length str))))

  (define (wr obj col)
    (define (wr-expr expr col)
      (if (read-macro? expr)
          (wr (read-macro-body expr) (out (read-macro-prefix expr) col))
          (wr-lst expr col)))

    (define (wr-lst l col)
      (if (pair? l)
          (let loop ((l (cdr l))
                     (col (and col (wr (car l) (out "(" col)))))
            (cond ((not col) col)
                  ((pair? l)
                   (loop (cdr l) (wr (car l) (out " " col))))
                  ((null? l) (out ")" col))
                  (else      (out ")" (wr l (out " . " col))))))
          (out "()" col)))

    (cond ((pair? obj)        (wr-expr obj col))
          ((null? obj)        (wr-lst obj col))
          ((vector? obj)      (wr-lst (vector->list obj) (out "#" col)))
          ((boolean? obj)     (out (if obj "#t" "#f") col))
          ((number? obj)      (out (number->string obj) col))
          ((symbol? obj)      (out (symbol->string obj) col))
          ((string? obj)      (if display?
                                  (out obj col)
                                  (let loop ((i 0) (j 0) (col (out "\"" col)))
                                    (if (and col (fx<? j (string-length obj)))
                                        (let ((c (string-ref obj j)))
                                          (if (or (char=? c #\\)
                                                  (char=? c #\"))
                                              (loop j
                                                    (fx+ j 1)
                                                    (out "\\"
                                                         (out (substring obj i j)
                                                              col)))
                                              (loop i (fx+ j 1) col)))
                                        (out "\""
                                             (out (substring obj i j) col))))))
          (else               (out (format (if display? "~a" "~s") obj) col))))

  (define (pp obj col)
    (define (spaces n col)
      (if (fx>? n 0)
          (if (fx>? n 7)
              (spaces (fx- n 8) (out "        " col))
              (out (substring "        " 0 n) col))
          col))

    (define (indent to col)
      (and col
           (if (fx<? to col)
               (and (out genwrite:newline-str col) (spaces to 0))
               (spaces (fx- to col) col))))

    (define (pr obj col extra pp-pair)
      (if (or (pair? obj) (vector? obj)) ; may have to split on multiple lines
          (let ((result '())
                (left (min (fx+ (fx- (fx- width col) extra) 1) (pretty-width))))
            (generic-write obj display? #f
                           (lambda (str)
                             (set! result (cons str result))
                             (set! left (fx- left (string-length str)))
                             (fx>? left 0)))
            (if (fx>? left 0) ; all can be printed on one line
                (out (reverse-string-append result) col)
                (if (pair? obj)
                    (pp-pair obj col extra)
                    (pp-list (vector->list obj) (out "#" col) extra pp-expr))))
          (wr obj col)))

    (define (pp-expr expr col extra)
      (if (read-macro? expr)
          (pr (read-macro-body expr)
              (out (read-macro-prefix expr) col)
              extra
              pp-expr)
          (let ((head (car expr)))
            (if (symbol? head)
                (let ((proc (style head)))
                  (if proc
                      (proc expr col extra)
                      (if (fx>? (string-length (symbol->string head))
                             (max-call-head-width))
                          (pp-general expr col extra #f #f #f pp-expr)
                          (pp-call expr col extra pp-expr))))
                (pp-list expr col extra pp-expr)))))

    ; (head item1
    ;       item2
    ;       item3)
    (define (pp-call expr col extra pp-item)
      (let ((col* (wr (car expr) (out "(" col))))
        (and col
             (pp-down (cdr expr) col* (fx+ col* 1) extra pp-item))))

    ; (item1
    ;  item2
    ;  item3)
    (define (pp-list l col extra pp-item)
      (let ((col (out "(" col)))
        (pp-down l col col extra pp-item)))

    (define (pp-down l col1 col2 extra pp-item)
      (let loop ((l l) (col col1))
        (and col
             (cond ((pair? l)
                    (let ((rest (cdr l)))
                      (let ((extra (if (null? rest) (fx+ extra 1) 0)))
                        (loop rest
                              (pr (car l) (indent col2 col) extra pp-item)))))
                   ((null? l)
                    (out ")" col))
                   (else
                    (out ")"
                         (pr l
                             (indent col2 (out "." (indent col2 col)))
                             (fx+ extra 1)
                             pp-item)))))))

    (define (pp-general expr col extra named? pp-1 pp-2 pp-3)
      (define (tail1 rest col1 col2 col3)
        (if (and pp-1 (pair? rest))
            (let* ((val1 (car rest))
                   (rest (cdr rest))
                   (extra (if (null? rest) (fx+ extra 1) 0)))
              (tail2 rest col1 (pr val1 (indent col3 col2) extra pp-1) col3))
            (tail2 rest col1 col2 col3)))
      (define (tail2 rest col1 col2 col3)
        (if (and pp-2 (pair? rest))
            (let* ((val1 (car rest))
                   (rest (cdr rest))
                   (extra (if (null? rest) (fx+ extra 1) 0)))
              (tail3 rest col1 (pr val1 (indent col3 col2) extra pp-2)))
            (tail3 rest col1 col2)))
      (define (tail3 rest col1 col2)
        (pp-down rest col2 col1 extra pp-3))
      (let* ((head (car expr))
             (rest (cdr expr))
             (col* (wr head (out "(" col))))
        (if (and named? (pair? rest))
            (let* ((name (car rest))
                   (rest (cdr rest))
                   (col** (wr name (out " " col*))))
              (tail1 rest (fx+ col (indent-general)) col** (fx+ col** 1)))
            (tail1 rest (fx+ col (indent-general)) col* (fx+ col* 1)))))

    (define (pp-expr-list l col extra)
      (pp-list l col extra pp-expr))
      
    (define (pp-SYNTAX-CASE expr col extra)
      (pp-general expr col extra #t pp-expr-list #f pp-expr))      

    (define (pp-LAMBDA expr col extra)
      (pp-general expr col extra #f pp-expr-list #f pp-expr))

    (define (pp-IF expr col extra)
      (pp-general expr col extra #f pp-expr #f pp-expr))

    (define (pp-COND expr col extra)
      (pp-call expr col extra pp-expr-list))

    (define (pp-CASE expr col extra)
      (pp-general expr col extra #f pp-expr #f pp-expr-list))
      
    (define (pp-CASE-LAMBDA expr col extra)
      (pp-general expr col extra #f #f #f pp-expr-list))      

    (define (pp-AND expr col extra)
      (pp-call expr col extra pp-expr))

    (define (pp-LET expr col extra)
      (let* ((rest (cdr expr))
             (named? (and (pair? rest) (symbol? (car rest)))))
        (pp-general expr col extra named? pp-expr-list #f pp-expr)))

    (define (pp-BEGIN expr col extra)
      (pp-general expr col extra #f #f #f pp-expr))

    (define (pp-DO expr col extra)
      (pp-general expr col extra #f pp-expr-list pp-expr-list pp-expr))

    (define (style head)
      (case head
        ((lambda let* letrec letrec* define define-syntax 
          syntax-rules let-syntax letrec-syntax with-syntax 
          library library-letrec*)   pp-LAMBDA)
        ((syntax-case)               pp-SYNTAX-CASE)
        ((if set! when unless)       pp-IF)
        ((cond)                      pp-COND)
        ((case-lambda)               pp-CASE-LAMBDA)
        ((case)                      pp-CASE)
        ((and or import export)      pp-AND)
        ((let)                       pp-LET)
        ((begin)                     pp-BEGIN)
        ((do)                        pp-DO)
        (else                        #f)))
        
    (pr obj col 0 pp-expr))

  (if width
      (out genwrite:newline-str (pp obj 0))
      (wr obj 0)))

; define formatting style (change these to suit your style)
      
(define pretty-width (make-parameter 72))
(define indent-general (make-parameter 2))
(define max-call-head-width (make-parameter 3))
(define pretty-gensyms (make-parameter #f))

(define (reverse-string-append l)
  (define (rev-string-append l i)
    (if (pair? l)
        (let* ((str (car l))
               (len (string-length str))
               (result (rev-string-append (cdr l) (fx+ i len))))
          (let loop ((j 0) (k (fx- (fx- (string-length result) i) len)))
            (if (fx<? j len)
                (begin
                  (string-set! result k (string-ref str j))
                  (loop (fx+ j 1) (fx+ k 1)))
                result)))
        (make-string i)))
  (rev-string-append l 0))
  
(define (pretty-print obj . opt)
  (let ((port (if (pair? opt) (car opt) (current-output-port))))
    (generic-write obj #f (pretty-width)
                   (lambda (s) (display s port) #t)))))