#| License
Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme io ports)
  (export
    file-options
    
    buffer-mode?
    
    latin-1-codec
    utf-8-codec
    utf-16-codec
    eol-style
    native-eol-style
    
    &i/o-decoding
    make-i/o-decoding-error
    i/o-decoding-error?
    &i/o-encoding
    make-i/o-encoding-error
    i/o-encoding-error?
    i/o-encoding-error-char
    
    error-handling-mode
    make-transcoder
    native-transcoder
    transcoder-codec
    transcoder-eol-style
    transcoder-error-handling-mode
    
    bytevector->string
    string->bytevector
    
    eof-object
    eof-object?
    
    port?
    port-transcoder
    textual-port?
    binary-port?
    transcoded-port
    
    port-has-port-position?
    port-position
    port-has-set-port-position!?
    set-port-position!
    
    close-input-port
    close-output-port
    close-port
    call-with-port
    
    input-port?
    port-eof?
    open-file-input-port
    open-bytevector-input-port
    open-string-input-port
    standard-input-port
    current-input-port
    make-custom-binary-input-port
    make-custom-textual-input-port
    
    get-u8
    lookahead-u8
    get-bytevector-n
    get-bytevector-n!
    get-bytevector-some
    get-bytevector-all
    
    get-char
    lookahead-char
    get-string-n
    get-string-n!
    get-string-all
    get-line
    get-datum
    
    output-port?
    flush-output-port
    output-port-buffer-mode
    open-file-output-port
    open-bytevector-output-port
    call-with-bytevector-output-port
    open-string-output-port
    call-with-string-output-port
    
    standard-output-port
    standard-error-port
    
    current-output-port
    current-error-port
    
    make-custom-binary-output-port
    make-custom-textual-output-port
    
    put-u8
    put-bytevector
    
    put-char
    put-string
    put-datum
    
    open-file-input/output-port
    
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port
    
    open-input-file
    open-output-file
    
    read
    
    open-output-string
    get-output-string
    
    annotation?
    annotation-expression
    annotation-source
    annotation-stripped
    )
  
  (import 
    (ironscheme clr)
    (ironscheme contracts)
    (ironscheme unsafe)
    (only (ironscheme reader) read-annotated)
    (only (psyntax compat) parameterize)
    (except (ironscheme) 
      parameterize
      latin-1-codec
      utf-8-codec
      utf-16-codec
      make-transcoder
      transcoder-codec
      transcoder-eol-style
      transcoder-error-handling-mode
      port-transcoder
      textual-port?
      binary-port?
      input-port?
      output-port?
      current-error-port
      current-input-port
      current-output-port
      native-transcoder
      open-input-file
      open-output-file
      close-input-port
      close-output-port
      read
      transcoded-port
      port-has-port-position?
      port-position
      port-has-set-port-position!?
      set-port-position!
      close-port
      port-eof?
      open-file-input-port
      call-with-port
      open-string-output-port 
      open-bytevector-input-port
      open-string-input-port
      make-custom-binary-input-port
      make-custom-textual-input-port
      get-u8
      lookahead-u8
      get-bytevector-n
      get-bytevector-n!
      get-bytevector-some
      get-bytevector-all
      get-char
      lookahead-char
      get-string-n
      get-string-n!
      get-string-all
      get-line
      flush-output-port
      output-port-buffer-mode
      open-bytevector-output-port
      call-with-bytevector-output-port
      make-custom-binary-output-port
      make-custom-textual-output-port
      put-u8
      put-bytevector
      put-char
      put-string
      make-custom-binary-input/output-port
      make-custom-textual-input/output-port
      
      textual-input-port?
      textual-output-port?      
      
      port? 
      call-with-string-output-port
      open-output-string
      get-output-string
      put-datum
      get-datum
      buffer-mode?
      native-eol-style
      standard-error-port
      standard-input-port
      standard-output-port))
  
  (clr-using System)
  (clr-using System.IO)
  (clr-using System.Text)
  ;(clr-using System.Text.RegularExpressions)
  (clr-using IronScheme.Runtime)  
  (clr-using IronScheme.Runtime.R6RS)
  (clr-using IronScheme.Runtime.psyntax)
  
  (define (codec? obj)
    (clr-is Encoding obj))
    
  (define/contract (get-codec name:string enccb deccb)
    (clr-static-call Encoding 
                     (GetEncoding String EncoderFallback DecoderFallback)
                     name 
                     enccb 
                     deccb))
                     
  (define (textual-input-port? obj)
    (and (input-port? obj)
         (textual-port? obj)))   
         
  (define (textual-output-port? obj)
    (and (output-port? obj)
         (textual-port? obj)))                        
    
  (define (latin-1-codec) 
    (clr-static-call Encoding (GetEncoding String) "iso-8859-1"))
    
  (define (utf-8-codec) 
    (clr-new UTF8Encoding #f))
    
  (define (utf-16-codec) 
    (clr-new UnicodeEncoding #t #f))
  
  (define (get-newline sym current)
    (case sym
      [(none)   current]
      [(lf)     "\n"]
      [(cr)     "r"]
      [(crlf)   "\r\n"]
      [(nel)    "\x0085;"]
      [(crnel)  "\r\x0085;"]
      [(ls)     "\x2028;"]
      [else
        (assertion-violation 'transcode-port "not a valid eof symbol" sym)]))
        
  (define/contract (codec-name codec:codec)
    (clr-prop-get Encoding WebName codec))
    
  (define (transcoder? obj)
    (clr-is Transcoder obj))
    
  (define (i/o-decode-error)
    (raise-continuable (make-i/o-decoding-error #f)))
    
  (define (i/o-encode-error)
    (raise-continuable (make-i/o-encoding-error #f #f)))
  
  (define/contract make-transcoder
    (case-lambda
      [(codec)
        (make-transcoder codec (native-eol-style))]
      [(codec eol)
        (make-transcoder codec eol 'replace)]
      [(codec:codec eol:symbol handling-mode:symbol)
        (clr-new Transcoder 
                 (if (eq? handling-mode 'raise)
                     (get-codec (codec-name codec) 
                                (clr-new EncCB i/o-encode-error)
                                (clr-new DecCB i/o-decode-error))
                     codec)
                 eol
                 handling-mode)]))
                     
  (define (native-transcoder)
    (clr-static-field-get Transcoder native))                     

  (define/contract (transcoder-codec tc:transcoder)
    (clr-field-get Transcoder codec tc))

  (define/contract (transcoder-eol-style tc:transcoder)
    (clr-field-get Transcoder eolstyle tc))

  (define/contract (transcoder-error-handling-mode tc:transcoder)
    (clr-field-get Transcoder handlingmode tc))
    
  (define (string-replace str old new)
    (clr-call String (Replace String String) str old new))
    
  ;(define (match-value m)
    ;(clr-prop-get Group Value m))
    
  (define (has-preamble? bv)
    (let ((b0 ($bytevector-ref bv 0))
          (b1 ($bytevector-ref bv 1)))
      (or (and (fx=? b0 #xff) (fx=? b1 #xfe))
          (and (fx=? b0 #xfe) (fx=? b1 #xff)))))
#|
  (define/contract (bytevector->string bv:bytevector tc:transcoder)
    (let ((l (bytevector-length bv)))
      (let-values (((s c) (if (and (> l 1) (has-preamble? bv))
                              (values 2 (- l 2))
                              (values 0 l))))
        (let ((value (clr-call Encoding GetString (transcoder-codec tc) bv s c))
              (eol   (transcoder-eol-style tc)))
          (if (eq? 'none eol)
              (string-replace value "\r" "")
              (regex-replace eoltx 
                             value
                             (lambda (m)
                               (get-newline 
                                  eol
                                  (match-value m)))))))))
            

  (define/contract (string->bytevector str:string tc:transcoder)
    ())
|#
  
  (define/contract (port-transcoder port:port) 
    (cond
      [(clr-is TranscodedReader port)
        (clr-prop-get TranscodedReader Transcoder port)]
      [(clr-is TranscodedWriter port)
        (clr-prop-get TranscodedWriter Transcoder port)]
      [(clr-is CustomTextReaderWriter port)
        (or (port-transcoder (get-output-port port))
            (port-transcoder (get-input-port port)))]
      [else #f]))
  
  (define (textual-port? obj)
    (or (clr-is TextReader obj)
        (clr-is TextWriter obj)
        (clr-is CustomTextReaderWriter obj)))
        
  (define (binary-port? obj)
    (clr-is Stream obj))
    
  (define (binary-input-port? obj)
    (and (clr-is Stream obj)
         (clr-prop-get Stream CanRead obj)))    
         
  (define (binary-output-port? obj)
    (and (clr-is Stream obj)
         (clr-prop-get Stream CanWrite obj)))    
    
  (define (input-port? obj)
    (or (binary-input-port? obj)
        (clr-is TextReader obj)
        (clr-is CustomTextReaderWriter obj)))
        
  (define (output-port? obj)
    (or (binary-output-port? obj)
        (clr-is TextWriter obj)
        (clr-is CustomTextReaderWriter obj)))
  
  (define current-input-port 
    (make-parameter (clr-static-prop-get Console In) 
                    (lambda (x) 
                      (unless (and (textual-port? x)
                                   (input-port? x))
                        (assertion-violation 'current-input-port
                                             "not a textual input port"
                                             x))
                      x)))

  (define current-output-port 
    (make-parameter (clr-static-prop-get Console Out) 
                    (lambda (x) 
                      (unless (and (textual-port? x)
                                   (output-port? x))
                        (assertion-violation 'current-output-port
                                             "not a textual output port"
                                             x))
                      x)))
                      
  (define current-error-port 
    (make-parameter (clr-static-prop-get Console Error) 
                    (lambda (x) 
                      (unless (and (textual-port? x)
                                   (output-port? x))
                        (assertion-violation 'current-error-port
                                             "not a textual output port"
                                             x))
                      x)))
   
  (define (standard-error-port)
    (clr-static-call Console OpenStandardError))
    
  (define (standard-input-port)
    (clr-static-call Console OpenStandardInput))

  (define (standard-output-port)
    (clr-static-call Console OpenStandardOutput))
  
  (define/contract (open-input-file filename:string)
    (unless (file-exists? filename)
      (file-not-found 'open-file-input-port filename))
    (clr-guard (e 
      [e (assertion-violation 'open-input-file "oops" filename)])
        (clr-static-call File OpenText filename)))

  (define/contract (open-output-file filename:string)
    (clr-guard (e 
      [e (assertion-violation 'open-output-file "oops" filename)])
        (clr-static-call File CreateText filename)))
  
  (define (get-input-port port)
    (clr-field-get CustomTextReaderWriter input port))
    
  (define (get-output-port port)
    (clr-field-get CustomTextReaderWriter output port))
    
  (define (close-input-port port)
    (cond
      [(clr-is Stream port) 
        (clr-call Stream Close port)]
      [(clr-is TextReader port)
        (clr-call TextReader Close port)]
      [(clr-is CustomTextReaderWriter port)
        (close-input-port (get-input-port port))]
      [else
        (assertion-violation 'close-input-port "not an input-port" port)]))
        
  (define (close-output-port port)
    (cond
      [(clr-is Stream port) 
        (clr-call Stream Close port)]
      [(clr-is TextWriter port)
        (clr-call TextWriter Close port)]
      [(clr-is CustomTextReaderWriter port)
        (close-output-port (get-output-port port))]
      [else
        (assertion-violation 'close-output-port "not an output-port" port)]))  
        
  (define (annotation? obj)
    (clr-is Annotation obj))
    
  (define/contract (annotation-expression anno:annotation)
    (clr-field-get Annotation expression anno))

  (define/contract (annotation-source anno:annotation)
    (clr-field-get Annotation source anno))

  (define/contract (annotation-stripped anno:annotation)
    (clr-field-get Annotation stripped anno))
    
  (define/contract read    
    (case-lambda
      [()
        (read (current-input-port))]
      [(port:textual-input-port)
        (let ((r (read-annotated port)))
          (if (annotation? r)
              (annotation-stripped r)
              r))]))
              
  (define/contract (transcoded-port port:binary-port tc:transcoder)
    (let ((r? (clr-prop-get Stream CanRead port))
          (w? (clr-prop-get Stream CanWrite port)))
      (if r?
          (if w?
              (clr-new CustomTextReaderWriter 
                       "textual/input-output-port"
                       (clr-new TranscodedReader port tc)
                       (clr-new TranscodedWriter port tc))
              (clr-new TranscodedReader port tc))
          (clr-new TranscodedWriter port tc))))
          
  (define (port-has-port-position? port)
    (cond
      [(clr-is CustomTextWriter port)
        (clr-prop-get CustomTextWriter HasPosition port)]
      [(clr-is CustomTextReader port)
        (clr-prop-get CustomTextReader HasPosition port)]
      [(clr-is CustomStream port)
        (clr-prop-get CustomStream HasPosition port)]
      [(clr-is Stream port)   #t]
      [(clr-is CustomTextReaderWriter port)
        (port-has-port-position? (get-input-port port))]
      [(clr-is TextWriter port) #f]
      [else
        (assertion-violation 'port-has-port-position? "not a port" port)]))
        
  (define-syntax long->fixnum
    (syntax-rules ()
      [(_ n)
        (clr-static-call Convert (ToInt32 Int64) n)]))
        
  (define (port-position port)
    (cond
      [(clr-is CustomTextReader port)
        (if (clr-prop-get CustomTextReader HasPosition port)
            (clr-prop-get CustomTextReader Position port)
            (assertion-violation 'port-position "not supplied to custom port" port))]
      [(clr-is CustomTextWriter port)
        (if (clr-prop-get CustomTextWriter HasPosition port)
            (clr-prop-get CustomTextWriter Position port)
            (assertion-violation 'port-position "not supplied to custom port" port))]
      [(clr-is CustomStream port)
        (if (clr-prop-get CustomStream HasPosition port)
            (long->fixnum (clr-prop-get Stream Position port))
            (assertion-violation 'port-position "not supplied to custom port" port))]
      [(clr-is Stream port)
        (long->fixnum (clr-prop-get Stream Position port))]
      [(clr-is CustomTextReaderWriter port)
        (port-position (get-input-port port))]
      [else
        (assertion-violation 'port-position "not supported" port)]))
        

  (define (port-has-set-port-position!? port)
    (cond
      [(clr-is CustomTextWriter port)
        (clr-prop-get CustomTextWriter HasSetPosition port)]
      [(clr-is CustomTextReader port)
        (clr-prop-get CustomTextReader HasSetPosition port)]
      [(clr-is Stream port)
        (clr-prop-get Stream CanSeek port)]
      [(clr-is CustomTextReaderWriter port)
        (port-has-set-port-position!? (get-input-port port))]
      [else
        (assertion-violation 'port-has-set-port-position!? "not a port" port)]))

  (define-syntax fixnum->long
    (syntax-rules ()
      [(_ n)
        (clr-static-call Convert (ToInt64 Int32) n)]))
        
  (define/contract (set-port-position! port pos:fixnum)        
    (cond
      [(clr-is CustomTextWriter port)
        (if (clr-prop-get CustomTextWriter HasSetPosition port)
            (clr-prop-set! CustomTextWriter Position port pos)
            (assertion-violation 'set-port-position! "not supplied to custom port" port))]
      [(clr-is CustomTextReader port)
        (if (clr-prop-get CustomTextReader HasSetPosition port)
            (clr-prop-set! CustomTextReader Position port pos)
            (assertion-violation 'set-port-position! "not supplied to custom port" port))]
      [(clr-is Stream port)
        (clr-prop-set! Stream Position port (fixnum->long pos))]
      [(clr-is CustomTextReaderWriter port)
        (set-port-position! (get-input-port port) pos)]
      [else
        (assertion-violation 'set-port-position! "not supported" port)]))
        
  (define (close-port port)
    (when (input-port? port)
      (close-input-port port))
    (when (output-port? port)
      (close-output-port port)))        
      
  (define/contract (port-eof? port:input-port)
    (eof-object?
      (cond
        [(clr-is Stream port)
          (lookahead-u8 port)]
        [(clr-is TextReader port)
          (lookahead-char port)]
        [(clr-is CustomTextReaderWriter port)
          (port-eof? (get-input-port port))]
        [else
          (assertion-violation 'port-eof? "not supported" port)])))
          
  (define (file-not-found who filename)
    (raise-continuable
      (condition
        (make-i/o-file-does-not-exist-error filename)
        (make-who-condition who))))
                
          
  (define/contract open-file-input-port
    (case-lambda
      [(filename)
        (open-file-input-port filename '())]
      [(filename options)
        (open-file-input-port filename options 'line)]
      [(filename options buffer-mode)
        (open-file-input-port filename options buffer-mode #f)]        
      [(filename:string options buffer-mode:buffer-mode tc)
        (unless (file-exists? filename)
          (file-not-found 'open-file-input-port filename))
        (clr-guard (e 
          [e (assertion-violation 'open-file-input-port "oops" filename)])
            (let ((s (clr-static-call File OpenRead filename)))
              (let ((s (if (eq? buffer-mode 'block)
                           (clr-new BufferedStream s)
                           s)))
                  (if tc
                      (transcoded-port s tc)
                      s))))]))

  (define/contract open-bytevector-input-port
    (case-lambda
      [(bv)
        (open-bytevector-input-port bv #f)]
      [(bv:bytevector tc)
        (let ((s (clr-new MemoryStream bv #f)))
          (if tc
              (transcoded-port s tc)
              s))]))

  (define/contract (open-string-input-port str:string)
    (clr-new StringReader (clr-cast String str)))  
    
  (define (proc/false? obj)
    (or (eq? obj #f)
        (procedure? obj)))  
    
  (define/contract 
      (make-custom-binary-input-port id:string 
                                     read:procedure 
                                     get-pos:proc/false 
                                     set-pos:proc/false 
                                     close:proc/false)
    (clr-new CustomBinaryInputStream id read get-pos set-pos close))
                    
  (define/contract 
      (make-custom-textual-input-port id:string 
                                      read:procedure 
                                      get-pos:proc/false 
                                      set-pos:proc/false 
                                      close:proc/false)
    (clr-new CustomTextReader id read get-pos set-pos close))
    
  (define (io-port-error who port)    
    (raise-continuable
      (condition
        (make-i/o-port-error port)
        (make-who-condition who))))
    
  (define/contract (get-u8 port:binary-input-port)
    (clr-guard (e [e (io-port-error 'lookahead-u8 port)])
      (let ((c (clr-call Stream ReadByte port)))
        (if (fx=? c -1) 
            (eof-object)
            c))))
      
  (define/contract (lookahead-u8 port:binary-input-port)
    (clr-guard (e [e (io-port-error 'lookahead-u8 port)])  
      (if (port-has-set-port-position!? port)
        (let ((c (clr-call Stream ReadByte port)))
          (if (fx=? c -1) 
              (eof-object)
              (begin 
                (set-port-position! port (- (port-position port) 1))
                c)))
        #f) ;; check me
      ))    
      
  (define-syntax read-buffer
    (syntax-rules ()
      [(_ port buffer start length)
        (clr-call Stream Read port buffer start length)])) 
        
  (define (trim-buffer bv k)
    (let ((nb (make-bytevector k)))
      (clr-static-call Array (Copy Array Array Int32) bv nb k)
      nb))
      
  (define/contract (get-bytevector-n port:binary-input-port count:fixnum)
    (clr-guard (e [e (io-port-error 'get-bytevector-n port)])
      (let* ((buffer (make-bytevector count))
             (r (read-buffer port buffer 0 count)))
        (if (fxzero? r)
            (eof-object)
            (let ((r (let f ((r r))
                      (if (fx=? r count)
                          r
                          (let ((rr (read-buffer port buffer r (fx- count r))))
                            (if (fxzero? rr)
                                (if (not (fxzero? r))
                                    r
                                    (eof-object))
                                (f (fx+ rr r))))))))
              (if (eof-object? r)
                  r                              
                  (if (not (fx=? r count))
                      (trim-buffer buffer r)
                      buffer)))))))
                      
  (define/contract (get-bytevector-n! port:binary-input-port bv:bytevector start:fixnum count:fixnum)
    (clr-guard (e [e (io-port-error 'get-bytevector-n! port)])
      (let* ((r (read-buffer port bv start count)))
        (if (fxzero? r)
            (eof-object)
            (let f ((r r))
              (if (fx=? r count)
                  r
                  (let ((rr (read-buffer port bv (fx+ r start) (fx- count r))))
                    (if (fxzero? rr)
                        (if (not (fxzero? r))
                            r
                            (eof-object))
                        (f (fx+ rr r))))))))))
                        
  (define/contract (get-bytevector-some port:binary-input-port)
    (clr-guard (e [e (io-port-error 'get-bytevector-some port)])
      (let f ((a '()))
        (let ((c (clr-call Stream ReadByte port)))
          (if (fx=? c -1)
              (if (null? a)
                  (eof-object)
                  (u8-list->bytevector (reverse a)))
              (f (cons c a)))))))
        
  (define/contract (get-bytevector-all port:binary-input-port)
    (clr-guard (e [e (io-port-error 'get-bytevector-all port)])
      (let f ((a '()))
        (let ((c (clr-call Stream ReadByte port)))
          (if (fx=? c -1)
              (if (null? a)
                  (eof-object)
                  (u8-list->bytevector (reverse a)))
              (f (cons c a)))))))
              
  (define/contract (get-char port:textual-input-port)
    (if (clr-is CustomTextReaderWriter port)
        (get-char (get-input-port port))
        (clr-guard (e [e (io-port-error 'get-char port)])
          (let ((c (clr-call TextReader Read port)))
            (if (fx=? c -1)
                (eof-object)
                (integer->char c))))))

  (define/contract (lookahead-char port:textual-input-port)
    (if (clr-is CustomTextReaderWriter port)
        (lookahead-char (get-input-port port))
        (clr-guard (e [e (io-port-error 'lookahead-char port)])
          (let ((c (clr-call TextReader Peek port)))
            (if (fx=? c -1)
                (eof-object)
                (integer->char c))))))

  (define/contract (get-string-n port:textual-input-port count:fixnum)
    (if (clr-is CustomTextReaderWriter port)
        (get-string-n (get-input-port port) count)  
        (clr-guard (e [e (io-port-error 'get-string-n port)])
          (let* ((buffer (clr-new-array Char count))
                 (c  (clr-call TextReader Read port buffer 0 count)))
            (if (fxzero? c)
                (eof-object)
                (let f ((c c))
                  (if (fx<? c count)
                      (let ((x (clr-call TextReader Read port buffer c (fx- count c))))
                        (if (fxzero? x)
                            (clr-new String buffer 0 c)
                            (f (fx+ c x))))
                      (clr-new String buffer 0 c))))))))

  (define/contract (get-string-n! port:textual-input-port str:string start:fixnum count:fixnum)
    (let* ((ss (get-string-n port count))
           (sl (string-length ss)))
      (do ((i 0 (fx+ i 1)))
        ((fx=? i sl) sl)
        (string-set! str 
                     (fx+ start i)
                     (string-ref ss i)))))
                     
  (define/contract (get-string-all port:textual-input-port)
    (if (clr-is CustomTextReaderWriter port)
        (get-string-all (get-input-port port))  
        (clr-guard (e 
            [e (io-port-error 'get-string-all port)])
          (let ((l (clr-call TextReader ReadToEnd port)))
            (if (null? l)
                (eof-object)
                l)))))

  (define/contract (get-line port:textual-input-port)
    (if (clr-is CustomTextReaderWriter port)
        (get-line (get-input-port port))  
        (clr-guard (e 
            [e (io-port-error 'get-line port)])
          (let ((l (clr-call TextReader ReadLine port)))
            (if (null? l)
                (eof-object)
                l)))))
                
  (define (flush-output-port port)
    (cond
      [(clr-is Stream port)
        (clr-call Stream Flush port)]
      [(clr-is TextWriter port)
        (clr-call TextWriter Flush port)]
      [(clr-is CustomTextReaderWriter port)
        (flush-output-port (get-output-port port))]
      [else
        (assertion-violation 'flush-output-port "not an output port" port)]))
        
  (define/contract (output-port-buffer-mode port:output-port)
    (if (clr-is BufferedStream port)
        'block
        'line)) 
    
  (define/contract open-bytevector-output-port
    (case-lambda
      [()
        (open-bytevector-output-port #f)]
      [(tc)
        (let ((s (clr-new MemoryStream)))
          (values (if tc
                      (transcoded-port s tc)
                      s)
                  (lambda ()
                    (let ((r (clr-call MemoryStream ToArray s)))
                      (clr-call MemoryStream SetLength s (clr-static-call Convert (ToInt64 Int32) 0))
                      r))))]))   
                      
  (define/contract call-with-bytevector-output-port
    (case-lambda
      [(p)
        (call-with-bytevector-output-port p #f)]
      [(p:procedure tc)
        (let-values (((s e) (open-bytevector-output-port tc)))
          (p s)
          (e))]))
          
  (define/contract 
      (make-custom-binary-output-port id:string 
                                      write:procedure 
                                      get-pos:proc/false 
                                      set-pos:proc/false 
                                      close:proc/false)
    (clr-new CustomBinaryOutputStream id write get-pos set-pos close))
                    
  (define/contract 
      (make-custom-textual-output-port id:string 
                                       write:procedure 
                                       get-pos:proc/false 
                                       set-pos:proc/false 
                                       close:proc/false)
    (clr-new CustomTextWriter id write get-pos set-pos close))          
    

  (define (->byte k)
    (unless (fixnum? k)
      (assertion-violation #f "not a fixnum" k))
    (when (or (fx<? k 0) (fx>? k 255))
      (assertion-violation #f "too big or small for byte" k))
    (clr-cast Byte (clr-cast Int32 k)))

  (define/contract (put-u8 port:binary-output-port byte:fixnum)
    (let ((byte (->byte byte)))
      (clr-guard (e [e (io-port-error 'put-u8 port)])
        (clr-call Stream WriteByte port byte))))
            
  (define/contract put-bytevector 
    (case-lambda
      [(port bv)
        (put-bytevector port bv 0)]
      [(port bv start)
        (put-bytevector port bv start (fx- (bytevector-length bv) start))]
      [(port:binary-output-port bv:bytevector start:fixnum count:fixnum)
        (clr-call Stream Write port bv start count)]))

  (define/contract (put-char port:textual-output-port chr:char)
    (if (clr-is CustomTextReaderWriter port)
        (put-char (get-output-port port) chr)  
        (clr-call TextWriter (Write Char) port chr)))
        
  (define (->string str)
    (cond
      [(clr-is String str) str]
      [(clr-is StringBuilder str) 
        (clr-call Object ToString str)]
      [else
        (assertion-violation '->string "not a string")]))        
        
  (define/contract put-string 
    (case-lambda
      [(port str)
        (put-string port str 0)]
      [(port str start)
        (put-string port str start (fx- (string-length str) start))]
      [(port:textual-output-port str:string start:fixnum count:fixnum)
        (if (clr-is CustomTextReaderWriter port)
            (put-string (get-output-port port) str start count)  
            (clr-call TextWriter 
                      (Write String)
                      port 
                      (clr-call String Substring (->string str) start count)))]))

  (define/contract 
      (make-custom-binary-input/output-port id:string 
                                            read:procedure
                                            write:procedure 
                                            get-pos:proc/false 
                                            set-pos:proc/false 
                                            close:proc/false)
    (clr-new CustomBinaryInputOutputStream id read write get-pos set-pos close))
                    
  (define/contract 
      (make-custom-textual-input/output-port id:string 
                                             read:procedure 
                                             write:procedure
                                             get-pos:proc/false 
                                             set-pos:proc/false 
                                             close:proc/false)
    (clr-new CustomTextReaderWriter id 
      (clr-new CustomTextReader id read get-pos set-pos close)
      (clr-new CustomTextWriter id write get-pos set-pos close)))
           
  (define (native-eol-style) 'crlf)
  
  (define (buffer-mode? obj)
    (and (symbol? obj) 
         (memq obj '(none line block)) 
         #t))

  (define/contract (put-datum p:textual-output-port datum) 
    (write datum p))
  
  (define/contract (get-datum port:textual-input-port) 
    (read port))
    
  (define (get-output-string port)
    (clr-call IronScheme.Runtime.StringWriter GetBuffer port))
    
  (define (open-output-string)
    (clr-new IronScheme.Runtime.StringWriter))
  
  (define (open-string-output-port)
    (let ((p (open-output-string)))
      (values p (lambda () (get-output-string p)))))
      
  (define (port? obj)
    (or (textual-port? obj) 
        (binary-port? obj)))
    
  (define/contract (call-with-string-output-port proc:procedure)
    (let ((p (open-output-string)))
      (call-with-port p proc)
      (get-output-string p)))
      
  (define/contract (call-with-port port:port proc:procedure)
    (let ((r (proc port)))
      (close-port port)
      r))      

)
