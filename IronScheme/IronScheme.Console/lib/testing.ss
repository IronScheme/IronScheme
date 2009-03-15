(library (testing)
	(export 
		test 
		get-errors
		clear-errors)
	(import
		(rnrs)
		(rnrs eval))

(define errors (make-eq-hashtable))

(define (get-errors) errors)

(define (clear-errors) (hashtable-clear! errors))

(define-syntax catch
	(syntax-rules ()
		[(_ body ...) 
			(with-exception-handler 
				(lambda (e) e) 
				(lambda () body ... ))]))

(define-syntax try
	(syntax-rules ()
		[(_ body ...) 
			(with-exception-handler 
				(lambda (e) (hashtable-set! errors (cadar '(body ...)) e) e) 
				(lambda () body ... ))]))
		
(define-syntax reval
	(syntax-rules ()
		[(_ e env* ...) (eval 'e (environment '(rnrs) env* ...))]))
		
(define-syntax test
	(syntax-rules	(=> &>)
		[(_ body ... &> expect) 
			(let ((e (catch (reval body ...))))
				(unless ((condition-predicate (record-type-descriptor expect)) e) 
					(hashtable-set! errors (car '(body ...)) (cons 'expect e))))]
		[(_ body ... => expect) (try (reval (assert (equal? (begin body ...) expect))))]
		[(_ body ...) (try (reval body ...))]))
)