#!r6rs
;;; FILE       "format.sls"
;;; IMPLEMENTS SRFI-48: Intermediary format strings
;;;            http://srfi.schemers.org/srfi-48/srfi-48.html
;;; AUTHOR     Ken Dickey
;;; UPDATED    Syntax updated for R6RS February 2008 by Ken Dickey
;;; LANGUAGE   R6RS but specific to Ikarus Scheme

;;;Copyright (C) Kenneth A Dickey (2003). All Rights Reserved.
;;;
;;;Permission is hereby granted, free of charge, to any person
;;;obtaining a copy of this software and associated documentation
;;;files (the "Software"), to deal in the Software without
;;;restriction, including without limitation the rights to use,
;;;copy, modify, merge, publish, distribute, sublicense, and/or
;;;sell copies of the Software, and to permit persons to whom
;;;the Software is furnished to do so, subject to the following
;;;conditions:
;;;
;;;The above copyright notice and this permission notice shall
;;;be included in all copies or substantial portions of the Software.
;;;
;;;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;OTHER DEALINGS IN THE SOFTWARE.


(library (srfi format)
  (export format)
  (import (rnrs)
          (only (ironscheme pretty-print) pretty-print)
          (srfi string-ports)
          (srfi parameters)
	)


; The implementation below requires SRFI-6 (Basic string ports), SRFI-23 (Error reporting mechanism) and SRFI-38 (External Representation for Data With Shared Structure). 
 

;; IMPLEMENTATION DEPENDENT options

(define ascii-tab   (integer->char  9))  ;; NB: assumes ASCII encoding
(define dont-print  (if (eq? #t #f) 1)) ;; ikarus
;;(define DONT-PRINT (string->symbol ""))
;;(define DONT-PRINT (void))
;;(define DONT-PRINT #!void)
;;(define pretty-print   write) ; ugly but permitted
;; (require 'srfi-38)  ;; write-with-shared-structure
(define write-with-shared-structure pretty-print) ;; !ikarus


;; FORMAT
(define (format . args)
  (cond
   [(null? args)
    (error 'format "required format-string argument is missing")
    ]
   [(string? (car args))
    (apply format (cons #f args))
    ]
   [(< (length args) 2)
    (error 'format "too few arguments" (cons 'format args))
    ]
   [else
    (let ( [output-port   (car  args)]
           [format-string (cadr args)]
           [args          (cddr args)]
         )
      (letrec ( [port 
                 (cond [(output-port? output-port) output-port]
                       [(eq? output-port #t) (current-output-port)]
                       [(eq? output-port #f) (open-output-string)]
                       [else (error 'format "bad output-port argument"
                                      output-port)])
                 ]
                [return-value 
                 (if (eq? output-port #f)    ;; if format into a string 
                     (lambda () (get-output-string port)) ;; then return the string
                     (lambda () dont-print)) ;; else do something harmless
                 ]       
              )  

         (define (string-index str c)
           (let ( [len (string-length str)] )
             (let loop ( [i 0] )
               (cond ((= i len) #f)
                     ((eqv? c (string-ref str i)) i)
                     (else (loop (+ i 1)))))))

         (define (string-grow str len char)
           (let ( [off (- len (string-length str))] )
             (if (positive? off)
               (string-append (make-string off char) str)
               str)))

         (define (compose-with-digits digits pre-str frac-str exp-str)
           (let ( [frac-len (string-length frac-str)] )
             (cond
              [(< frac-len digits) ;; grow frac part, pad with zeros
               (string-append pre-str "."
                              frac-str (make-string (- digits frac-len) #\0)
                              exp-str)
               ]
              [(= frac-len digits) ;; frac-part is exactly the right size
               (string-append pre-str "."
                              frac-str
                              exp-str)
               ]
              [else ;; must round to shrink it
               (let* ( [first-part (substring frac-str 0 digits)]
                       [last-part  (substring frac-str digits frac-len)]
                       [temp-str
                        (number->string
                         (round (string->number
                                 (string-append first-part "." last-part))))]
                       [dot-pos (string-index  temp-str #\.)]
                       [carry?
                        (and (> dot-pos digits)
                             (> (round (string->number
                                        (string-append "0." frac-str)))
                                0))]
                       [new-frac
                        (substring temp-str 0 digits)]
                     )
                 (string-append
                  (if carry? (number->string (+ 1 (string->number pre-str))) pre-str)
                  "."
                  new-frac
                  exp-str))]
         ) ) )
 
         (define (format-fixed number-or-string width digits) ; returns a string
           (cond
            [(string? number-or-string)
             (string-grow number-or-string width #\space)
             ]
            [(number? number-or-string)
             (let ( [real (real-part number-or-string)]
                    [imag (imag-part number-or-string)]
                  )
               (cond
                [(not (zero? imag))
                 (string-grow
                  (string-append (format-fixed real 0 digits)
                                 (if (negative? imag) "" "+")
                                 (format-fixed imag 0 digits)
                                 "i")
                  width
                  #\space)
                 ]
                [digits
                 (let* ( [num-str   (number->string (exact real))]
                         [dot-index (string-index  num-str #\.)]
                         [exp-index (string-index  num-str #\e)]
                         [length    (string-length num-str)]
                         [pre-string
                          (cond
                           (exp-index
                            (if dot-index
                                (substring num-str 0 dot-index)
                                (substring num-str 0 (+ exp-index 1)))
                            )
                           (dot-index
                            (substring num-str 0 dot-index)
                            )
                           (else
                            num-str))
                          ]
                         [exp-string
                          (if exp-index (substring num-str exp-index length) "")
                          ]
                         [frac-string
                          (if exp-index
                              (substring num-str (+ dot-index 1) exp-index)
                              (substring num-str (+ dot-index 1) length))
                          ]
                       )
                   (string-grow
                    (if dot-index
                        (compose-with-digits digits
                                             pre-string
                                             frac-string
                                             exp-string)
                        (string-append pre-string exp-string))
                    width
                    #\space)
                 )]
                [else ;; no digits
                 (string-grow (number->string real) width #\space)])
             )]
            [else
             (error 'format "~F requires a number or a string" number-or-string)])
           )

         (define documentation-string
"(format [<port>] <format-string> [<arg>...]) -- <port> is #t, #f or an output-port
OPTION  [MNEMONIC]      DESCRIPTION     -- Implementation Assumes ASCII Text Encoding
~H      [Help]          output this text
~A      [Any]           (display arg) for humans
~S      [Slashified]    (write arg) for parsers
~W      [WriteCircular] like ~s but outputs circular and recursive data structures
~~      [tilde]         output a tilde
~T      [Tab]           output a tab character
~%      [Newline]       output a newline character
~&      [Freshline]     output a newline character if the previous output was not a newline
~D      [Decimal]       the arg is a number which is output in decimal radix
~X      [heXadecimal]   the arg is a number which is output in hexdecimal radix
~O      [Octal]         the arg is a number which is output in octal radix
~B      [Binary]        the arg is a number which is output in binary radix
~w,dF   [Fixed]         the arg is a string or number which has width w and d digits after the decimal
~C      [Character]     charater arg is output by write-char
~_      [Space]         a single space character is output
~Y      [Yuppify]       the list arg is pretty-printed to the output
~?      [Indirection]   recursive format: next 2 args are format-string and list of arguments
~K      [Indirection]   same as ~?
"
          )

         (define (require-an-arg args)
           (when (null? args)
             (error 'format "too few arguments"))
         )
        
         (define (format-help format-strg arglist)
          
          (letrec (
             [length-of-format-string (string-length format-strg)]
             
             [anychar-dispatch       
              (lambda (pos arglist last-was-newline) 
                (if (>= pos length-of-format-string) 
                  arglist ; return unused args 
                  (let ( [char (string-ref format-strg pos)] ) 
                    (cond            
                     [(eqv? char #\~)   
                      (tilde-dispatch (+ pos 1) arglist last-was-newline)]
                     [else                   
                      (write-char char port)     
                      (anychar-dispatch (+ pos 1) arglist #f)        
                      ])             
                    )))     
              ] ; end anychar-dispatch

             [has-newline?
              (lambda (whatever last-was-newline)
                (or (eqv? whatever #\newline)
                    (and (string? whatever)
                         (let ( [len (string-length whatever)] )
                           (if (zero? len)
                               last-was-newline
                               (eqv? #\newline (string-ref whatever (- len 1)))))))
              )] ; end has-newline?
             
             [tilde-dispatch          
              (lambda (pos arglist last-was-newline)     
                (cond           
                 ((>= pos length-of-format-string)   
                  (write-char #\~ port) ; tilde at end of string is just output
                  arglist ; return unused args
                  )     
                 (else      
                  (case (char-upcase (string-ref format-strg pos)) 
                    ((#\A)       ; Any -- for humans
                     (require-an-arg arglist)
                     (let ( [whatever (car arglist)] )
                       (display whatever port)
                       (anychar-dispatch (+ pos 1) 
                                         (cdr arglist) 
                                         (has-newline? whatever last-was-newline))
                     ))
                    ((#\S)       ; Slashified -- for parsers
                     (require-an-arg arglist)
                     (let ( [whatever (car arglist)] )
                        (write whatever port)     
                        (anychar-dispatch (+ pos 1) 
                                          (cdr arglist) 
                                          (has-newline? whatever last-was-newline)) 
                     ))
                    ((#\W)
                     (require-an-arg arglist)
                     (let ( [whatever (car arglist)] )
                        (write-with-shared-structure whatever port)  ;; srfi-38
                        (anychar-dispatch (+ pos 1) 
                                          (cdr arglist) 
                                          (has-newline? whatever last-was-newline))
                     ))                           
                    ((#\D)       ; Decimal
                     (require-an-arg arglist)
                     (display (number->string (car arglist) 10) port)  
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f)  
                     )            
                    ((#\X)       ; HeXadecimal
                     (require-an-arg arglist)
                     (display (number->string (car arglist) 16) port)
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f)  
                     )             
                    ((#\O)       ; Octal
                     (require-an-arg arglist)
                     (display (number->string (car arglist)  8) port) 
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f) 
                     )       
                    ((#\B)       ; Binary
                     (require-an-arg arglist)
                     (display (number->string (car arglist)  2) port)
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f) 
                     )           
                    ((#\C)       ; Character
                     (require-an-arg arglist)
                     (write-char (car arglist) port) 
                     (anychar-dispatch (+ pos 1)
                                       (cdr arglist)
                                       (eqv? (car arglist) #\newline))  
                     )          
                    ((#\~)       ; Tilde  
                     (write-char #\~ port)   
                     (anychar-dispatch (+ pos 1) arglist #f) 
                     )            
                    ((#\%)       ; Newline   
                     (newline port) 
                     (anychar-dispatch (+ pos 1) arglist #t) 
                     )
                    ((#\&)      ; Freshline
                     (if (not last-was-newline) ;; (unless last-was-newline ..
                         (newline port))
                     (anychar-dispatch (+ pos 1) arglist #t)
                     )
                    ((#\_)       ; Space 
                     (write-char #\space port)   
                     (anychar-dispatch (+ pos 1) arglist #f)
                     )             
                    ((#\T)       ; Tab -- IMPLEMENTATION DEPENDENT ENCODING    
                     (write-char ascii-tab port)          
                     (anychar-dispatch (+ pos 1) arglist #f)     
                     )             
                    ((#\Y)       ; Pretty-print
                     (pretty-print (car arglist) port)  ;; IMPLEMENTATION DEPENDENT
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f)
                     )              
                    ((#\F)
                     (require-an-arg arglist)
                     (display (format-fixed (car arglist) 0 #f) port)
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f)
                     )
                    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                     ;; gather "~w[,d]F" w and d digits
                     (let loop ( [index (+ pos 1)]
                                 [w-digits (list (string-ref format-strg pos))]
                                 [d-digits '()]
                                 [in-width? #t]
                               )
                       (if (>= index length-of-format-string)
                           (error 'format
                                  "improper numeric format directive" format-strg)
                           (let ( [next-char (string-ref format-strg index)] )
                             (cond
                              [(char-numeric? next-char)
                               (if in-width?
                                   (loop (+ index 1)
                                         (cons next-char w-digits)
                                         d-digits
                                         in-width?)
                                   (loop (+ index 1)
                                         w-digits
                                         (cons next-char d-digits)
                                         in-width?))
                               ]
                              [(char=? next-char #\F)
                               (let ( [width
                                       (string->number
                                        (list->string (reverse w-digits)))]
                                      [digits
                                       (if (zero? (length d-digits))
                                           #f
                                           (string->number
                                            (list->string (reverse d-digits))))]
                                    )
                                 (display
                                  (format-fixed (car arglist) width digits)
                                  port)
                                 (anychar-dispatch (+ index 1) (cdr arglist) #f))
                               ]
                              [(char=? next-char #\,)
                               (if in-width?
                                   (loop (+ index 1)
                                         w-digits
                                         d-digits
                                         #f)
                                   (error 'format
                                          "too many commas in directive"
                                          format-strg))
                               ]
                              [else
                               (error 'format "~w.dF directive ill-formed"
                                      format-strg)])))
                     ))
                    ((#\? #\K)       ; indirection -- take next arg as format string
                     (cond           ;  and following arg as list of format args
                      ((< (length arglist) 2)
                       (error 'format
                              "less arguments than specified for ~?" arglist)
                       )
                      ((not (string? (car arglist)))
                       (error 'format "~? requires a string" (car arglist))
                       )
                      (else
                       (format-help (car arglist) (cadr arglist))
                       (anychar-dispatch (+ pos 1) (cddr arglist) #f)
                     )))
                    ((#\H)      ; Help
                     (display documentation-string port)
                     (anychar-dispatch (+ pos 1) arglist #t)
                     )
                    (else                
                     (error 'format "unknown tilde escape"
                                    (string-ref format-strg pos)))
                    )))
                )] ; end tilde-dispatch   
             ) ; end letrec            
            
             ; format-help main      
             (anychar-dispatch 0 arglist #f) 
            )) ; end format-help    
        
        ; format main
        (let ( [unused-args (format-help format-string args)] )
          (if (not (null? unused-args))
              (error 'format "unused arguments" unused-args)
              (return-value)))
                                              
        )) ; end letrec
    ]) ; end else, cond
)  ; end format

)


