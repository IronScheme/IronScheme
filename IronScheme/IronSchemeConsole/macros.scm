
(define-syntax when
  (syntax-rules ()
    ((when test e1 e2 ...)
     (if test
	 (begin e1 e2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test e1 e2 ...)
     (if (not test)
	 (begin e1 e2 ...)))))

; while test do e1 e2 ... end

(define-syntax while
  (syntax-rules ()
    ((while test e1 e2 ...)
     (letrec ((loop
	       (lambda ()
		 (if test
		     (begin e1 e2 ... (loop))))))
       (loop)))))

; repeat e1 e2 ... until test

(define-syntax repeat
  (syntax-rules ()
    ((repeat test e1 e2 ...)
     (letrec ((loop
	       (lambda ()
		 e1 e2 ... (if (not test) (loop)))))
       (loop)))))