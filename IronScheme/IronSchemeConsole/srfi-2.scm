; 			Checking of a LAND* special form
;
; LAND* is a generalized AND: it evaluates a sequence of forms one after another
; till the first one that yields #f; the non-#f result of a form can be bound
; to a fresh variable and used in the subsequent forms.
;
; When an ordinary AND is formed of _proper_ boolean expressions:
;	(AND E1 E2 ...)
; expression E2, if it gets to be evaluated, knows that E1 has returned non-#f.
; Moreover, E2 knows exactly what the result of E1 was - #t - so E2 can use
; this knowledge to its advantage. If E1 however is an _extended_
; boolean expression, E2 can no longer tell which particular non-#f
; value E1 has returned. Chances are it took a lot of work to evaluate E1,
; and the produced result (a number, a vector, a string, etc) may be of
; value to E2. Alas, the AND form merely checks that the result is not an #f,
; and throws it away. If E2 needs it, it has to recompute the value again.
; This proposed LAND* special form lets constituent expressions get
; hold of the results of already evaluated expressions, without re-doing
; their work.
;
; Syntax:
; 	LAND* (CLAWS) BODY
;
; where CLAWS is a list of expressions or bindings: 
;	CLAWS ::= '() | (cons CLAW CLAWS)
; Every element of the CLAWS list, a CLAW, must be one of the following:
;	(VARIABLE EXPRESSION)
; or
;	(EXPRESSION)
; or
;	BOUND-VARIABLE
; These CLAWS are evaluated in the strict left-to-right order. For each
; CLAW, the EXPRESSION part is evaluated first (or BOUND-VARIABLE is looked up).
; If the result is #f, LAND* immediately returns #f, thus disregarding the rest
; of the CLAWS and the BODY. If the EXPRESSION evaluates to not-#f, and
; the CLAW is of the form
;	(VARIABLE EXPRESSION)
; the EXPRESSION's value is bound to a freshly made VARIABLE. The VARIABLE is
; available for _the rest_ of the CLAWS, and the BODY. As usual, all
; VARIABLEs must be unique (like in let*).
;
; Thus LAND* is a sort of cross-breed between LET* and AND.
;
; Denotation semantics:
;
; Eval[ (LAND* (CLAW1 ...) BODY), Env] =
;	EvalClaw[ CLAW1, Env ] andalso 
;		Eval[ (LAND* ( ...) BODY), ExtClawEnv[ CLAW1, Env]]
;
; Eval[ (LAND* (CLAW) ), Env] = EvalClaw[ CLAW, Env ]
; Eval[ (LAND* () FORM1 ...), Env] = Eval[ (BEGIN FORM1 ...), Env ]
; Eval[ (LAND* () ), Env] = #t
;
; EvalClaw[ BOUND-VARIABLE, Env ] = Eval[ BOUND-VARIABLE, Env ]
; EvalClaw[ (EXPRESSION), Env ] = Eval[ EXPRESSION, Env ]
; EvalClaw[ (VARIABLE EXPRESSION), Env ] = Eval[ EXPRESSION, Env ]
;
; ExtClawEnv[ BOUND-VARIABLE, Env ] = Env
; ExtClawEnv[ (EXPRESSION), Env ] = EnvAfterEval[ EXPRESSION, Env ]
; ExtClawEnv[ (VARIABLE EXPRESSION), Env ] = 
;	ExtendEnv[ EnvAfterEval[ EXPRESSION, Env ],
;		   VARIABLE boundto Eval[ EXPRESSION, Env ]]
;
;
; If one has a Scheme interpreter written in Prolog/ML/Haskell, he can
; implement the above semantics right away. Within Scheme, it is trivial to
; code LAND* with R4RS "define-syntax". Alas, Gambit does not have this
; facility. So this implementation uses macros instead.
;
; The following LAND* macro will convert a LAND* expression into a "tree" of
; AND and LET expressions. For example,
; (LAND* ((my-list (compute-list)) ((not (null? my-list))))
;	(do-something my-list))
; is transformed into
; (and (let ((my-list (compute-list)))
;	(and my-list  (not (null? my-list)) (begin (do-something my-list)))))
;
; I must admit the LAND* macro is written in a pathetic anti-functional style.
; To my excuse, the macro's goal is a syntactic transformation of source
; code, that is, performing a re-writing. IMHO, rewriting kind of suggests
; mutating.
;
; Sample applications:
;
; The following piece of code (from my treap package) 
;   (let ((new-root (node:dispatch-on-key root key ...)))
;      (if new-root (set! root new-root)))
; could be elegantly re-written as
;   (land* ((new-root (node:dispatch-on-key root key ...)))
;		(set! root new-root))
;
; A very common application of land* is looking up a value
; associated with a given key in an assoc list, returning #f in case of a
; look-up failure:
;
;		; Standard implementation
; (define (look-up key alist)
;   (let ((found-assoc (assq key alist)))
;	(and found-assoc (cdr found-assoc))))
;
; 		; A more elegant solution
; (define (look-up key alist)
;   (cdr (or (assq key alist) '(#f . #f))))
;
; 		; An implementation which is just as graceful as the latter
;		; and just as efficient as the former:
; (define (look-up key alist)
;   (land* ((x (assq key alist))) (cdr x)))
;
; Generalized cond:
;
; (or
;   (land* (bindings-cond1) body1)
;   (land* (bindings-cond2) body2)
;   (begin else-clause))
;
; Unlike => (cond's send), LAND* applies beyond cond. LAND* can also be used
; to generalize cond, as => is limited to sending of only a single value;
; LAND* allows as many bindings as necessary (which are performed in sequence)
;
; (or
;  (land* ((c (read-char)) ((not (eof-object? c))))
;	(string-set! some-str i c) (++! i))
;  (begin (do-process-eof)))
;
; Another concept LAND* is reminiscent of is programming with guards:
; a LAND* form can be considered a sequence of _guarded_ expressions.
; In a regular program, forms may produce results, bind them to variables
; and let other forms use these results. LAND* differs in that it checks
; to make sure that every produced result "makes sense" (that is, not an #f).
; The first "failure" triggers the guard and aborts the rest of the
; sequence (which presumably would not make any sense to execute anyway).
;
; $Id: vland-gambit.scm,v 1.1 1998/12/28 23:54:29 srfimgr Exp $


(define-macro (LAND* claws . body)
  (let* ((new-vars '()) (result (cons 'and '())) (growth-point result))

			; We need a way to report a syntax error
			; the following is how Gambit compiler does it...
    (##define-macro (ct-error-syntax msg . args)
      `(##signal '##signal.syntax-error #t ,msg ,@args))

    (define (andjoin! clause)
      (let ((prev-point growth-point) (clause-cell (cons clause '())))
        (set-cdr! growth-point clause-cell)
        (set! growth-point clause-cell)))

    (if (not (list? claws))
      (ct-error-syntax "bindings must be a list " bindings))
    (for-each 
      (lambda (claw)
        (cond
          ((symbol? claw)	; BOUND-VARIABLE form
            (andjoin! claw))
          ((and (pair? claw) (null? (cdr claw))) ; (EXPRESSION) form
            (andjoin! (car claw)))
						; (VARIABLE EXPRESSION) form
          ((and (pair? claw) (symbol? (car claw))
              (pair? (cdr claw)) (null? (cddr claw)))
            (let* ((var (car claw)) (var-cell (cons var '())))
              (if (memq var new-vars)
                (ct-error-syntax "duplicate variable " var " in the bindings"))
              (set! new-vars (cons var new-vars))
              (set-cdr! growth-point `((LET (,claw) (AND . ,var-cell))))
              (set! growth-point var-cell)))
          (else
            (ct-error-syntax "An ill-formed binding in a syntactic form land* " 
              claw))
        ))
      claws)
    (if (not (null? body))
      (andjoin! `(begin ,@body)))
    result))


	; Validation tests
(##include "myenv.scm")
(##include "catch-error.scm")

(display "\nValidating LAND*...\n\n")

		; make sure that the 'FORM' gave upon evaluation the
		; EXPECTED-RESULT
(define-macro (expect form expected-result)
  `(begin
    (display "evaluating ")
    (write ',form)
    (let ((real-result (eval ',form)))
     (if (equal? real-result ,expected-result)
       (cout "... gave the expected result: " real-result nl)
       (error "... yielded: " real-result
        " which differs from the expected result: " ,expected-result)
      ))))

		; Check to see that 'form' has indeed a wrong syntax
(define-macro (must-be-a-syntax-error form)
  `(call-with-current-continuation
    (lambda (k)
      (##catch-signal '##signal.syntax-error
        (lambda x 
          (display "catching a syntax error: ") (display x) (newline)
          (k #f))
        (lambda ()
          (eval ',form)
          (error "No syntax error detected, unexpectedly"))))))

(expect  (land* () 1) 1)
(expect  (land* () 1 2) 2)
(expect  (land* () ) #t)

(expect (let ((x #f)) (land* (x))) #f)
(expect (let ((x 1)) (land* (x))) 1)
(expect (land* ((x #f)) ) #f)
(expect (land* ((x 1)) ) 1)
(must-be-a-syntax-error (land* ( #f (x 1))) )
(expect (land* ( (#f) (x 1)) ) #f)
(must-be-a-syntax-error (land* (2 (x 1))) )
(expect (land* ( (2) (x 1)) ) 1)
(expect (land* ( (x 1) (2)) ) 2)
(expect (let ((x #f)) (land* (x) x)) #f)
(expect (let ((x "")) (land* (x) x)) "")
(expect (let ((x "")) (land* (x)  )) "")
(expect (let ((x 1)) (land* (x) (+ x 1))) 2)
(expect (let ((x #f)) (land* (x) (+ x 1))) #f)
(expect (let ((x 1)) (land* (((positive? x))) (+ x 1))) 2)
(expect (let ((x 1)) (land* (((positive? x))) )) #t)
(expect (let ((x 0)) (land* (((positive? x))) (+ x 1))) #f)
(expect (let ((x 1)) (land* (((positive? x)) (x (+ x 1))) (+ x 1)))  3)
(must-be-a-syntax-error
  (let ((x 1)) (land* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1)))
)

(expect (let ((x 1)) (land* (x ((positive? x))) (+ x 1))) 2)
(expect (let ((x 1)) (land* ( ((begin x)) ((positive? x))) (+ x 1))) 2)
(expect (let ((x 0)) (land* (x ((positive? x))) (+ x 1))) #f)
(expect (let ((x #f)) (land* (x ((positive? x))) (+ x 1))) #f)
(expect (let ((x #f)) (land* ( ((begin x)) ((positive? x))) (+ x 1))) #f)

(expect  (let ((x 1)) (land* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  (let ((x 0)) (land* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  (let ((x #f)) (land* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  (let ((x 3)) (land* (x (y (- x 1)) ((positive? y))) (/ x y))) 3/2)

(display "\nAll tests passed\n")

(let
  ((a-definition
      '(define (bbb) 
        (LAND* ((my-list (compute-list)) a-condition ((not (null? my-list)))
            (my-list-tail (cdr my-list)))
          (do-something my-list-tail)))))
  (cout "The result of compiling of\n" 
    (lambda () (pp a-definition)) "\nis the following\n")
  (eval a-definition)
  (pp bbb)
)
