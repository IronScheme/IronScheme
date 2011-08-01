
(library (ironscheme debugger)
  (export step-into)
  (import 
    (ironscheme)
    (ironscheme clr)
    (ironscheme regex-cond)
    (ironscheme strings)
    (ironscheme console))
    
  (clr-using Microsoft.Scripting.Debugging)
  
  (define (stack-frame-filename sf)
    (clr-prop-get StackFrame Filename sf))
    
  (define (stack-frame-start-line sf)
    (clr-prop-get StackFrame StartLine sf))

  (define (stack-frame-start-column sf)
    (clr-prop-get StackFrame StartColumn sf))

  (define (stack-frame-end-line sf)
    (clr-prop-get StackFrame EndLine sf))

  (define (stack-frame-end-column sf)
    (clr-prop-get StackFrame EndColumn sf))
    
  (define (span-start-line span)
    (clr-prop-get #f Line
      (clr-prop-get Microsoft.Scripting.SourceSpan Start span)))

  (define (span-start-column span)
    (clr-prop-get #f Column
      (clr-prop-get Microsoft.Scripting.SourceSpan Start span)))

  (define (span-end-line span)
    (clr-prop-get #f Line
      (clr-prop-get Microsoft.Scripting.SourceSpan End span)))

  (define (span-end-column span)
    (clr-prop-get #f Column
      (clr-prop-get Microsoft.Scripting.SourceSpan End span)))
      
  (define (current-location)
    (car (reverse (lw-debugger-location-trace))))
    

  (define (file->vector filename)
    (let ((input (open-input-file filename)))
      (let f ((a '()))
        (let ((line (get-line input)))
          (if (eof-object? line)
              (begin (close-input-port input)
                     (list->vector (reverse a)))
              (f (cons line a)))))))

  (define source-map (make-eqv-hashtable))

  (define (get-source filename)
    (let ((src (hashtable-ref source-map filename #f)))
      (or src
          (let ((src (file->vector filename)))
            (hashtable-set! source-map filename src)
            src)))) 
            
  (define (display-bright str)
    (parameterize [(foreground-color 'Cyan)]
      (display str)))
            
  (define (print-source src sl sc el ec padding)
    (when (not (fxzero? padding))
      (let f ((line (fxmax 0 (fx- sl padding))))
        (unless (fx=? line sl)
          (printf "~a:" (fx+ line 1))
          (display (vector-ref src line))
          (newline)
          (f (fx+ line 1)))))
    (cond 
      [(fx=? sl el)
        (let* ((str (vector-ref src sl))
               (len (string-length str)))
          ; print line number
          (printf "~a:" (fx+ sl 1))
          ; print leading
          (display (substring str 0 sc))
          ; print middle
          (display-bright (substring str sc ec))
          ; print trailing
          (display (substring str ec len))
          (newline))]
      [else
        ; print first line
        (let* ((str (vector-ref src sl))
               (len (string-length str)))
          ; print line number
          (printf "~a:" (fx+ sl 1))
          ; print leading
          (display (substring str 0 sc))
          ; print middle
          (display-bright (substring str sc len))
          (newline))
        ; print middle
        (let f ((line (fx+ sl 1)))
          (unless (fx=? line el)
            ; print line number
            (printf "~a:" (fx+ line 1))
            ; print middle
            (display-bright (vector-ref src line))
            (newline)
            (f (fx+ line 1))))
        ; print last line  
        (let* ((str (vector-ref src el))
               (len (string-length str)))
          ; print line number
          (printf "~a:" (fx+ el 1))
          ; print middle
          (display-bright (substring str 0 ec))
          ; print trailing
          (display (substring str ec len))
          (newline))])
    (when (not (fxzero? padding))
      (let* ((vl (vector-length src))
             (maxline (fxmin (fx- vl 1) (fx+ el padding))))
        (let f ((line (fx+ el 1)))
          (unless (fx>? line maxline)
            (printf "~a:" (fx+ line 1))
            (display (vector-ref src line))
            (newline)
            (f (fx+ line 1)))))))
   
  (define debug-mode 'run)  
  
  (define (current-stack-frame)
    (car (lw-debugger-call-stack)))
  
  (define (for-each-variable proc)
    (let ((env (lw-debugger-stackframe-variables (current-stack-frame))))
      (let f ((i 0))
        (unless (fx=? i (vector-length env))
          (let ((e (vector-ref env i)))
            (hashtable-for-each e (lambda (k v)
                                    (proc k v i))))
          (f (fx+ i 1))))))
  
  (define (enter-debug-repl filename sl sc el ec)
    (print-source (get-source filename) sl sc el ec 0)
    (let loop ()
      (display-bright "debug> ")
      (let ((input (get-line (current-input-port))))
        (regex-cond* input 
          ["c(ontinue)?" (set! debug-mode 'run)]
          ["n(ext)?" (set! debug-mode 'step-into)]
          ["callstack"
            (for-each (lambda (x) 
                        (display x)
                        (newline))
                      (lw-debugger-call-stack))
            (loop)]
          ["w(here)?" 
            (print-source (get-source filename) sl sc el ec 2)
            (loop)]
          ["p(rint)?\\s+(?<varname>\\w+)"
            (let ((var (string->symbol varname)))
              (for-each-variable (lambda (k v i)
                                   (when (eq? (ungensym k) var)
                                     (printf "~a: ~a = ~s\n" i var v)))))
            (loop)]
          ["p(rint)?"
            (for-each-variable (lambda (k v i)
                                 (printf "~a: ~a = ~s\n" i (ungensym k) v)))
            (loop)]            
          [else 
            (unless (string=? input "")
              (printf "ERROR: Unknown command: ~a\n" input))
            (loop)])
         )))

  (define (notify reason filename startline startcol endline endcol)
    (when (and (memq reason '(expr-in expr-in-tail)) filename (fx>? startline 0))
      (if (eq? debug-mode 'step-into) 
          (enter-debug-repl filename 
                            (fx- startline 1)
                            (fx- startcol 1)
                            (fx- endline 1)
                            (fx- endcol 1))
          #f)))
          
  (define (handle-exception e)
    (printf "Unhandled exception:\n~a" e) ; already newline at end :(
    (let ((sf (current-stack-frame))
          (l (current-location)))
      (enter-debug-repl (stack-frame-filename sf) 
                        (fx- (span-start-line l) 1)
                        (fx- (span-start-column l) 1)
                        (fx- (span-end-line l) 1)
                        (fx- (span-end-column l) 1))))
            
  (define (reset)
    (set! debug-mode 'run)
    (hashtable-clear! source-map))           
            
  (define (step-into filename)
    (parameterize [(lw-debugger notify)]
      (reset)
      (set! debug-mode 'step-into)
      (guard [e [e (handle-exception e)]]
        (load filename))
      )))
