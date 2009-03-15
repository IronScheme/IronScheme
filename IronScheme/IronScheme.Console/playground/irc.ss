(import
  (ironscheme)
  (ironscheme strings)
  (ironscheme regex)
  (ironscheme threading))
  
(define-syntax regex-cond
  (lambda (x)
    (define (get-groups pattern)
      (map (lambda (match)
             (match-value (match-group match "group")))
           (regex-matches pattern "\\(\\?\\<(?<group>\\w+)\\>")))
    (syntax-case x (else)
      [(ctx str (pattern e e* ...) ... (else ee ee* ...))
        (let ((parse-clause (lambda (c)
                              (syntax-case c ()
                                [(pattern e e* ...)
                                  (let ((grps (get-groups (syntax->datum #'pattern))))
                                    (with-syntax (((binding ...) (map (lambda (p) 
                                                                        (datum->syntax #'ctx (string->symbol p))) 
                                                                      grps))
                                                  ((group ...) grps))
                                      #'((let ((m (regex-match str pattern)))
                                          (if (match-success? m)
                                              (let ((binding (match-value (match-group m group))) ...)
                                                e e* ...)
                                              #f)))))]))))
          (with-syntax (((clause ...) (map parse-clause #'((pattern e e* ...) ...))))
            #'(cond
                clause ...
                [else ee ee* ...])))]
      [(ctx str (pattern e e* ...) ... )
        #'(ctx str (pattern e e* ...) ... (else #f))])))


(define (handle-notice from msg)
  (printf "NOTICE from ~a msg ~a\n" from msg))
  
(define (handle-ping sender)
  (void))  
  
(define (handle-generic sender cmd target msg)
  (case cmd
    [("PRIVMSG") (printf "| ~a ~a ~a\n" target sender msg)]
    [else
      (printf "Unhandled: ~a ~a ~a ~a\n" sender cmd target msg)]))

(define (match-line line)
  (unless 
    (regex-cond line
      ["PING\\s:(?<sender>.+)" (handle-ping sender)]
      ["NOTICE\\s(?<from>.+):(?<msg>.*)"  (handle-notice from msg)]
      [":(?<sender>.+)\\s(?<cmd>.+)\\s(?<target>.+)\\s:(?<msg>.*)" (handle-generic sender cmd target msg)]
      [":(?<sender>.+)\\s(?<cmd>.+)\\s:(?<msg>.*)" (handle-generic sender cmd #f msg)]
      [":(?<sender>.+)\\s004\\s(?<target>.+)\\s(?<msg>.*)" (handle-generic sender "004" target msg)])
    (printf "not matched: ~a\n" line)))

(define ns (open-tcp-input/output-port "chat.freenode.net" 8000 (native-transcoder)))

(define (send-msg msg . args)
  (apply fprintf ns msg args)
  (newline ns)
  (flush-output-port ns))

(define read-thread
  (make-thread
    (lambda ()
      (let loop ((line (get-line ns)))
        (cond
          [(eof-object? line)
            (display "eof reached, exiting read thread\n")]
          [else
            (match-line line)
            (loop (get-line ns))])))))

(send-msg "USER ironscheme 8 * :ironscheme bot")
(send-msg "NICK ironscheme")
(send-msg "JOIN #ironscheme")

(start-thread read-thread)


