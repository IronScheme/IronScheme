(library (ironscheme format)
  (export printf
          fprintf
          format)
  (import (rnrs))

  ;; dofmt does all of the work.  It loops through the control string
  ;; recognizing format directives and printing all other characters
  ;; without interpretation.  A tilde at the end of a control string is
  ;; treated as an ordinary character.  No checks are made for proper
  ;; inputs.  Directives may be given in either lower.
  (define dofmt
    (lambda (p cntl args)
      (let ((nmax (fx- (string-length cntl) 1)))
        (let loop ((n 0) (a args))
          (if (fx<=? n nmax)
              (let ((c (string-ref cntl n)))
                (if (and (char=? c #\~) (fx<? n nmax))
                    (case (string-ref cntl (fx+ n 1))
                      ((#\a)
                       (display (car a) p)
                       (loop (fx+ n 2) (cdr a)))
                      ((#\s)
                       (write (car a) p)
                       (loop (fx+ n 2) (cdr a)))
                      ((#\b)
                       (write (number->string (car a) 2) p)
                       (loop (fx+ n 2) (cdr a)))
                      ((#\o)
                       (write (number->string (car a) 8) p)
                       (loop (fx+ n 2) (cdr a)))
                      ((#\x)
                       (write (number->string (car a) 16) p)
                       (loop (fx+ n 2) (cdr a)))
                      ((#\d)
                       (write (number->string (car a) 10) p)
                       (loop (fx+ n 2) (cdr a)))
                      ((#\~)
                       (write-char #\~ p)
                       (loop (fx+ n 2) a))
                      (else
                       (write-char c p)
                       (loop (fx+ n 1) a)))
                    (begin
                      (write-char c p)
                      (loop (fx+ n 1) a))))))))) 

  ;; printf and fprintf differ only in that fprintf passes its
  ;; port argument to dofmt while printf passes the current output
  ;; port.
  (define (printf control . args)
      (dofmt (current-output-port) control args))
      
  (define (fprintf p control . args)
      (dofmt p control args))
      
  (define (format control . args)
    (call-with-values open-string-output-port
      (lambda (p c)
        (dofmt p control args)
        (c))))
) 
    