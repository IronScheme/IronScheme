#!r6rs

(library (wak trc-testing limited-write)
  (export limited-write)
  (import (rnrs))

(define (write-string s port)
  (put-string port s))

;; Code below taken from scheme48 1.8, Copyright (c) 1993-2008 Richard
;; Kelsey and Jonathan Rees. Licensed under the new-style BSD license.

(define (limited-write obj port max-depth max-length)
  (let recur ((obj obj) (depth 0))
    (if (and (= depth max-depth)
	     (not (or (boolean? obj)
		      (null? obj)
		      (number? obj)
		      (symbol? obj)
		      (char? obj)
		      (string? obj))))
	(display "#" port)
	(call-with-current-continuation
	  (lambda (escape)
	    (recurring-write obj port
	      (let ((count 0))
		(lambda (sub)
		  (if (= count max-length)
		      (begin (display "---" port)
			     (write-char
			      (if (or (pair? obj) (vector? obj))
				  #\)
				  #\})
			      port)
			     (escape #t))
		      (begin (set! count (+ count 1))
			     (recur sub (+ depth 1))))))))))))

(define (recurring-write obj port recur)
  (cond ((pair? obj) (write-list obj port recur))
        ((vector? obj) (write-vector obj port recur))
        (else
         (write obj port))))

(define (write-list obj port recur)
  (cond ((quotation? obj)
         (write-char #\' port)
         (recur (cadr obj)))
        (else
         (write-char #\( port)
         (recur (car obj))
         (let loop ((l (cdr obj))
                    (n 1))
              (cond ((not (pair? l))
                     (cond ((not (null? l))
                            (write-string " . " port)
                            (recur l))))
                    (else
                      (write-char #\space port)
                      (recur (car l))
                      (loop (cdr l) (+ n 1)))))
         (write-char #\) port))))

(define (quotation? obj)
  (and (pair? obj)
       (eq? (car obj) 'quote)
       (pair? (cdr obj))
       (null? (cddr obj))))

(define (write-vector obj port recur)
   (write-string "#(" port)
   (let ((z (vector-length obj)))
     (cond ((> z 0)
            (recur (vector-ref obj 0))
            (let loop ((i 1))
              (cond ((>= i z))
                    (else
                     (write-char #\space port)
                     (recur (vector-ref obj i))
                     (loop (+ i 1))))))))
   (write-char #\) port))

)
