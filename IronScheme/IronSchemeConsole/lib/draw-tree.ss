; Print trees representing Scheme data.
; Copyright (C) 2006,2007 Nils M Holm
; Ported to R6RS - leppie 2008

(library (draw-tree)
  (export draw-tree)
  (import (rnrs))

; mark empty slots in lists
(define nothing
  (let ((N (cons 'N '())))
    (lambda () N)))

(define (empty? x)
  (eq? (nothing) x))

; mark partially processed lists
(define ls
  (let ((L (cons 'L '())))
    (lambda () L)))

(define (list-done? x)
  (and (eq? (ls) (car x))
       (null? (cdr x))))

(define (draw-string s)
  (let* ((k (string-length s))
         (s (if (> k 7) (substring s 0 7) s))
         (s (if (< k 3) (string-append " " s) s))
         (k (string-length s)))
    (display (string-append s
               (substring "        " 0
                 (- 8 (min k 7)))))))

(define (draw-atom n)
  (cond
    ((null? n)
      (draw-string "()"))
    ((symbol? n)
      (draw-string (symbol->string n)))
    ((number? n)
      (draw-string (number->string n)))
    ((string? n)
      (draw-string (string-append "\"" n "\"")))
    ((char? n)
      (draw-string (string-append "#\\" (string n))))
    ((eq? n #t)
      (draw-string "#t"))
    ((eq? n #f)
      (draw-string "#f"))
    (else
      (error 'draw-atom "unknown type" n))))

(define (draw-conses n sicp)
  (letrec
    ((draw-c
       (lambda (n)
         (cond
           ((not (pair? n))
             (draw-atom n))
           ((and sicp (null? (cdr n)))
             (display "[o|/]"))
           (else
             (display "[o|o]---")
             (draw-c (cdr n)))))))
    (draw-c n)
    (cons (ls) n)))

(define (draw-bars n)
  (cond
    ((not (pair? n))
      '())
    ((empty? (car n))
      (draw-string "")
      (draw-bars (cdr n)))
    ((and (pair? (car n)) (eq? (ls) (caar n)))
      (draw-bars (cdar n))
      (draw-bars (cdr n)))
    (else
      (draw-string "|")
      (draw-bars (cdr n)))))

(define (trim n)
  (letrec
    ((_trim
       (lambda (n)
         (cond
           ((null? n)
             '())
           ((empty? (car n))
             (_trim (cdr n)))
           ((list-done? (car n))
             (_trim (cdr n)))
           (else
             (reverse n))))))
    (_trim (reverse n))))

(define (draw-objects n sicp)
  (letrec
    ((draw-o
       (lambda (n r)
         (cond
           ((not (pair? n))
             (trim (reverse r)))
           ((empty? (car n))
             (draw-string "")
             (draw-o (cdr n)
                     (cons (nothing) r)))
           ((not (pair? (car n)))
             (draw-atom (car n))
             (draw-o (cdr n)
                     (cons (nothing) r)))
           ((null? (cdr n))
             (draw-o (cdr n)
                     (cons (draw-row (car n) sicp) r)))
           (else
             (draw-string "|")
             (draw-o (cdr n)
                     (cons (car n) r)))))))
    (cons (ls) (draw-o (cdr n) '()))))

(define (draw-row n sicp)
  (letrec
    ((draw-r
       (lambda (n r)
         (cond
           ((null? n)
             (reverse r))
           ((not (pair? (car n)))
             (draw-atom (car n))
             (draw-r (cdr n)
                     (cons (nothing) r)))
           ((eq? (ls) (caar n))
             (draw-r (cdr n)
                     (cons (draw-objects (car n) sicp)
                           r)))
           (else
             (draw-r (cdr n)
                     (cons (draw-conses (car n) sicp)
                           r)))))))
    (car (draw-r (list n) '()))))

(define (draw-tree n . sicp)
  (let ((sicp (and (not (null? sicp)) (car sicp))))
    (letrec
      ((draw-t
         (lambda (n)
           (cond ((list-done? n) '())
             (else
               (newline)
               (draw-bars (cdr n))
               (newline)
               (draw-t (draw-row n sicp)))))))
      (cond
        ((not (pair? n))
          (draw-atom n)
          (newline))
        (else
          (draw-t (draw-row n sicp))
          (newline))))))
)