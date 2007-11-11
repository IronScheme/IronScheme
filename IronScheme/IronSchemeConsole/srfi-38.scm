;;; A printer that shows all sharing of substructures.  Uses the Common
;;; Lisp print-circle notation: #n# refers to a previous substructure
;;; labeled with #n=.   Takes O(n^2) time.


  (define (write-with-shared-structure obj . optional-port)
    (define (acons key val alist)
      (cons (cons key val) alist))
    (define outport (if (eq? '() optional-port)
			(current-output-port)
			(car optional-port)))
    ;; We only track duplicates of pairs, vectors, and strings.  We
    ;; ignore zero-length vectors and strings because r5rs doesn't
    ;; guarantee that eq? treats them sanely (and they aren't very
    ;; interesting anyway).

    (define (interesting? obj)
      (or (pair? obj)
	  (and (vector? obj) (not (zero? (vector-length obj))))
	  (and (string? obj) (not (zero? (string-length obj))))))
    ;; (write-obj OBJ ALIST):
    ;; ALIST has an entry for each interesting part of OBJ.  The
    ;; associated value will be:
    ;;  -- a number if the part has been given one,
    ;;  -- #t if the part will need to be assigned a number but has not been yet,
    ;;  -- #f if the part will not need a number.
    ;; The cdr of ALIST's first element should be the most recently
    ;; assigned number.
    ;; Returns an alist with new shadowing entries for any parts that
    ;; had numbers assigned.
    (define (write-obj obj alist)
      (define (write-interesting alist)
	(cond ((pair? obj)
	       (display "(" outport)
	       (let write-cdr ((obj (cdr obj)) (alist (write-obj (car obj) alist)))
		 (cond ((and (pair? obj) (not (cdr (assq obj alist))))
			(display " " outport)
			(write-cdr (cdr obj) (write-obj (car obj) alist)))
		       ((null? obj)
			(display ")" outport)
			alist)
		       (else
			(display " . " outport)
			(let ((alist (write-obj obj alist)))
			  (display ")" outport)
			  alist)))))
	      ((vector? obj)
	       (display "#(" outport)
	       (let ((len (vector-length obj)))
		 (let write-vec ((i 1) (alist (write-obj (vector-ref obj 0) alist)))
		   (cond ((= i len) (display ")" outport) alist)
			 (else (display " " outport)
			       (write-vec (+ i 1)
					  (write-obj (vector-ref obj i) alist)))))))
	      ;; else it's a string
	      (else (write obj outport) alist)))
      (cond ((interesting? obj)
	     (let ((val (cdr (assq obj alist))))
	       (cond ((not val) (write-interesting alist))
		     ((number? val) 
		      (begin (display "#" outport)
			     (write val outport)
			     (display "#" outport) alist))
		     (else
		      (let ((n (+ 1 (cdar alist))))
			(begin (display "#" outport)
			       (write n outport) 
			       (display "=" outport))
			(write-interesting (acons obj n alist)))))))
	    (else (write obj outport) alist)))

    ;; Scan computes the initial value of the alist, which maps each
    ;; interesting part of the object to #t if it occurs multiple times,
    ;; #f if only once.
    (define (scan obj alist)
      (cond ((not (interesting? obj)) alist)
	    ((assq obj alist)
             => (lambda (p) (if (cdr p) alist (acons obj #t alist))))
	    (else
	     (let ((alist (acons obj #f alist)))
	       (cond ((pair? obj) (scan (car obj) (scan (cdr obj) alist)))
		     ((vector? obj)
		      (let ((len (vector-length obj)))
			(do ((i 0 (+ 1 i))
			     (alist alist (scan (vector-ref obj i) alist)))
			    ((= i len) alist))))
		     (else alist))))))
    (write-obj obj (acons 'dummy 0 (scan obj '())))
    ;; We don't want to return the big alist that write-obj just returned.
    (if #f #f))





(define (read-with-shared-structure . optional-port)
  (define port
    (if (null? optional-port) (current-input-port) (car optional-port)))

  (define (read-char*) (read-char port))
  (define (peek-char*) (peek-char port))

  (define (looking-at? c)
    (eqv? c (peek-char*)))

  (define (delimiter? c)
    (case c
      ((#\( #\) #\" #\;) #t)
      (else (or (eof-object? c)
		(char-whitespace? c)))))

  (define (not-delimiter? c) (not (delimiter? c)))

  (define (eat-intertoken-space)
    (define c (peek-char*))
    (cond ((eof-object? c))
	  ((char-whitespace? c) (read-char*) (eat-intertoken-space))
	  ((char=? c #\;)
	   (do ((c (read-char*) (read-char*)))
	       ((or (eof-object? c) (char=? c #\newline))))
	   (eat-intertoken-space))))

  (define (read-string)
    (read-char*)
    (let read-it ((chars '()))
      (let ((c (read-char*)))
	(if (eof-object? c)
	    (error "EOF inside a string")
	    (case c
	      ((#\") (list->string (reverse chars)))
	      ((#\\) (read-it (cons (read-char*) chars)))
	      (else (read-it (cons c chars))))))))

  ;; reads chars that match PRED and returns them as a string.
  (define (read-some-chars pred)
    (let iter ((chars '()))
      (let ((c (peek-char*)))
	(if (or (eof-object? c) (not (pred c)))
	    (list->string (reverse chars))
	    (iter (cons (read-char*) chars))))))

  ;; reads a character after the #\ has been read.
  (define (read-character)
    (let ((c (peek-char*)))
      (cond ((eof-object? c) (error "EOF inside a character"))
	    ((char-alphabetic? c)
	     (let ((name (read-some-chars char-alphabetic?)))
	       (cond ((= 1 (string-length name)) (string-ref name 0))
		     ((string-ci=? name "space") #\space)
		     ((string-ci=? name "newline") #\newline)
		     (else (error "Unknown named character: " name)))))
	    (else (read-char*)))))

  (define (read-number first-char)
    (let ((str (string-append (string first-char)
			      (read-some-chars not-delimiter?))))
      (or (string->number str)
	  (error "Malformed number: " str))))

  (define char-standard-case
    (if (char=? #\a (string-ref (symbol->string 'a) 0))
	char-downcase
	char-upcase))

  (define (string-standard-case str)
    (let* ((len (string-length str))
	   (new (make-string len)))
      (do ((i 0 (+ i 1)))
	  ((= i len) new)
	(string-set! new i (char-standard-case (string-ref str i))))))

  (define (read-identifier)
    (string->symbol (string-standard-case (read-some-chars not-delimiter?))))

  (define (read-part-spec)
    (let ((n (string->number (read-some-chars char-numeric?))))
      (let ((c (read-char*)))
	(case c
	  ((#\=) (cons 'decl n))
	  ((#\#) (cons 'use n))
	  (else (error "Malformed shared part specifier"))))))

  ;; Tokens: strings, characters, numbers, booleans, and
  ;; identifiers/symbols are represented as themselves.
  ;; Single-character tokens are represented as (CHAR), the
  ;; two-character tokens #( and ,@ become (#\#) and (#\@).
  ;; #NN= and #NN# become (decl . NN) and (use . NN).
  (define (read-optional-token)
    (eat-intertoken-space)
    (let ((c (peek-char*)))
      (case c
	((#\( #\) #\' #\`) (read-char*) (list c))
	((#\,)
	 (read-char*)
	 (if (looking-at? #\@)
	     (begin (read-char*) '(#\@))
	     '(#\,)))
	((#\") (read-string))
	((#\.)
	 (read-char*)
	 (cond ((delimiter? (peek-char*)) '(#\.))
	       ((not (looking-at? #\.)) (read-number #\.))
	       ((begin (read-char*) (looking-at? #\.)) (read-char*) '...)
	       (else (error "Malformed token starting with \"..\""))))
	((#\+) (read-char*) (if (delimiter? (peek-char*)) '+ (read-number c)))
	((#\-) (read-char*) (if (delimiter? (peek-char*)) '- (read-number c)))
	((#\#)
	 (read-char*)
	 (let ((c (peek-char*)))
	   (case c
	     ((#\() (read-char*) '(#\#))
	     ((#\\) (read-char*) (read-character))
	     ((#\t #\T) (read-char*) #t)
	     ((#\f #\F) (read-char*) #f)
	     (else (cond ((eof-object? c) (error "EOF inside a # token"))
			 ((char-numeric? c) (read-part-spec))
			 (else (read-number #\#)))))))
	(else (cond ((eof-object? c) c)
		    ((char-numeric? c) (read-char*) (read-number c))
		    (else (read-identifier)))))))

  (define (read-token)
    (let ((tok (read-optional-token)))
      (if (eof-object? tok)
	  (error "EOF where token was required")
	  tok)))

  ;; Parts-alist maps the number of each part to a thunk that returns the part.
  (define parts-alist '())

  (define (add-part-to-alist! n thunk)
    (set! parts-alist (cons (cons n thunk) parts-alist)))

  ;; Read-object returns a datum that may contain some thunks, which
  ;; need to be replaced with their return values.
  (define (read-object)
    (finish-reading-object (read-token)))

  ;; Like read-object, but may return EOF.
  (define (read-optional-object)
    (finish-reading-object (read-optional-token)))

  (define (finish-reading-object first-token)
    (if (not (pair? first-token))
	first-token
	(if (char? (car first-token))
	    (case (car first-token)
	      ((#\() (read-list-tail))
	      ((#\#) (list->vector (read-list-tail)))
	      ((#\. #\)) (error (string-append "Unexpected \"" first-token "\"")))
	      (else
	       (list (caadr (assv (car first-token)
				  '((#\' 'x) (#\, ,x) (#\` `x) (#\@ ,@x))))
		     (read-object))))
	    ;; We need to specially handle chains of declarations in
	    ;; order to allow #1=#2=x and #1=(#2=#1#) and not to allow
	    ;; #1=#2=#1# nor #1=#2=#1=x.
	    (let ((starting-alist parts-alist))
	      (let read-decls ((token first-token))
		(if (and (pair? token) (symbol? (car token)))
		    (let ((n (cdr token)))
		      (case (car token)
			((use)
			 ;; To use a part, it must have been
			 ;; declared before this chain started.
			 (cond ((assv n starting-alist) => cdr)
			       (else (error "Use of undeclared part " n))))
			((decl)
			 (if (assv n parts-alist)
			     (error "Double declaration of part " n))
			 ;; Letrec enables us to make deferred
			 ;; references to an object before it exists.
			 (letrec ((obj (begin
					 (add-part-to-alist! n (lambda () obj))
					 (read-decls (read-token)))))
			   obj))))
		    (finish-reading-object token)))))))

  (define (read-list-tail)
    (let ((token (read-token)))
      (if (not (pair? token))
	  (cons token (read-list-tail))
	  (case (car token)
	    ((#\)) '())
	    ((#\.) (let* ((obj (read-object))
			  (tok (read-token)))
		     (if (and (pair? tok) (char=? #\) (car tok)))
			 obj
			 (error "Extra junk after a dot"))))
	    (else (let ((obj (finish-reading-object token)))
		    (cons obj (read-list-tail))))))))

  ;; Unthunk.
  ;; To deference a part that was declared using another part,
  ;; e.g. #2=#1#, may require multiple dethunkings.  We were careful
  ;; in finish-reading-object to ensure that this won't loop forever:
  (define (unthunk thunk)
    (let ((x (thunk)))
      (if (procedure? x) (unthunk x) x)))

  (let ((obj (read-optional-object)))
    (let fill-in-parts ((obj obj))
      (cond ((pair? obj)
	     (if (procedure? (car obj))
		 (set-car! obj (unthunk (car obj)))
		 (fill-in-parts (car obj)))
	     (if (procedure? (cdr obj))
		 (set-cdr! obj (unthunk (cdr obj)))
		 (fill-in-parts (cdr obj))))
	    ((vector? obj)
	     (let ((len (vector-length obj)))
	       (do ((i 0 (+ i 1)))
		   ((= i len))
		 (let ((elt (vector-ref obj i)))
		   (if (procedure? elt)
		       (vector-set! obj i (unthunk elt))
		       (fill-in-parts elt))))))))
    obj))
