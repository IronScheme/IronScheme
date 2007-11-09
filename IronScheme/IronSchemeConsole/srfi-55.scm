;;;; Reference implementation for SRFI-55
;
; Requirements: SRFI-23 (error reporting)

(define available-extensions '())

(define (register-extension id action . compare)
  (set! available-extensions
    (cons (list (if (pair? compare) (car compare) equal?)
		id 
		action)
	  available-extensions)) )

(define (find-extension id)
  (define (lookup exts)
    (if (null? exts)
	(error "extension not found - please contact your vendor" id)
	(let ((ext (car exts)))
	  (if ((car ext) (cadr ext) id)
	      ((caddr ext))
	      (lookup (cdr exts)) ) ) ) )
  (lookup available-extensions) )

(define-syntax require-extension 
  (syntax-rules (srfi)
    ((_ "internal" (srfi id ...))
     (begin (find-extension '(srfi id) ...)) )
    ((_ "internal" id)
     (find-extension 'id) )
    ((_ clause ...)
     (begin (require-extension "internal" clause) ...)) ) )

; Example of registering extensions:
;
;   (register-extension '(srfi 1) (lambda () (load "/usr/local/lib/scheme/srfi-1.scm")))

