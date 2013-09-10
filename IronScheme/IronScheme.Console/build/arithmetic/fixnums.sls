#| License
Copyright (c) 2007-2013 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme arithmetic fixnums)
  (export
    fixnum?

    fixnum-width
    least-fixnum
    greatest-fixnum

    fx=?
    fx>?
    fx<?
    fx>=?
    fx<=?

    fxzero?
    fxpositive?
    fxnegative?
    fxodd?
    fxeven?

    fxmax
    fxmin

    fx+
    fx*
    fx-
    fxdiv-and-mod
    fxdiv
    fxmod
    fxdiv0-and-mod0
    fxdiv0
    fxmod0

    fxnot
    fxand
    fxior
    fxxor
    fxif
    fxbit-count
    fxlength
    fxfirst-bit-set
    fxbit-set?
    fxcopy-bit
    fxbit-field
    fxcopy-bit-field
    fxarithmetic-shift
    fxarithmetic-shift-left
    fxarithmetic-shift-right
    fxrotate-bit-field
    fxreverse-bit-field
    fxadd1
    fxsub1)
  (import 
    (ironscheme core)
    (ironscheme clr)
    (ironscheme typed)
    (ironscheme unsafe)
    (ironscheme integrable)
    (except (ironscheme) 
      fixnum-width
      fxif
      fxcopy-bit
      fxbit-field
      fxcopy-bit-field
      fxarithmetic-shift
      fxarithmetic-shift-left
      fxarithmetic-shift-right
      fxrotate-bit-field
 
      fxbit-count
      fxlength
      fxfirst-bit-set
      fxbit-set?

      fxmod
      fxmod0
      fxdiv-and-mod
      fxdiv0-and-mod0
      fxdiv0
      fxdiv

      fxand
      fxior
      fxxor

      fx=?
      fx<?
      fx>?
      fx<=?
      fx>=?

      fxnot

      fxzero?
      fxpositive?
      fxnegative?
      fxodd?
      fxeven?

      greatest-fixnum
      least-fixnum

      fxmax
      fxmin

      fx-
      fx+
      fx*
      fxreverse-bit-field    
      fxadd1
      fxsub1))

  (define (fixnum-width) 32)
  
  (define (greatest-fixnum)  #x7fffffff)
  (define (least-fixnum)    #x-80000000)

  (define-syntax checked
    (syntax-rules ()
      [(_ expr)
        (or expr (overflow-error #f))]))
        
  (define-syntax define-fx
    (lambda (x)
      (syntax-case x ()
        [(_ (name formals ...) body body* ...)
          (with-syntax (((formals* ...) (generate-temporaries #'(formals ...)))
                        ((type ...) (map (lambda (x) (datum->syntax x 'Int32)) #'(formals ...))))
            (with-syntax (((checks ...) 
              (map (lambda (f)
                     (with-syntax ((f f))
                       #'(unless (fixnum? f) 
                          (assertion-violation 'name "not a fixnum" f))))
                    #'(formals* ...))))
              #'(define (name formals* ...)
                  checks ...
                  ((typed-lambda (formals ...) ((type ...) Object) body body* ...)
                    formals* ...))))]))) 

  (define-syntax define-fx*
    (lambda (x)
      (syntax-case x ()
        [(_ (name formals ...) body body* ...)
          (with-syntax ((uname 
            (datum->syntax #'name
              (string->symbol
                (string-append 
                  (symbol->string (syntax->datum #'name))
                  "*")))))
            #'(begin
                (define-integrable (uname formals ...) body body* ...)
                (define-fx (name formals ...) (uname formals ...))))])))

  (define-syntax fxabs
    (syntax-rules ()
      [(_ e) (clr-static-call Math (Abs Int32) e)]))
      
  (define-fx (fxadd1 x)
    ($fx+ x 1))      

  (define-fx (fxsub1 x)
    ($fx- x 1))

  (define-fx (fx+ x1 x2)
    (checked (fx+internal x1 x2)))

  (define-fx (fx* x1 x2)
    (checked (fx*internal x1 x2)))

  (define fx-
    (case-lambda
      [(x1)
        (unless (fixnum? x1)
          (assertion-violation 'fx- "not a fixnum" x1))
        (let: ((x1 : Int32 x1))
          (when ($fx=? (least-fixnum) x1)
            (overflow-error 'fx- x1))
          ($fx- x1))]
      [(x1 x2)
        (unless (fixnum? x1)
          (assertion-violation 'fx- "not a fixnum" x1))
        (unless (fixnum? x2)
          (assertion-violation 'fx- "not a fixnum" x2))
        (checked (fx-internal x1 x2))]))

  (define (overflow-error name . irritants)
    (raise
      (condition
        (make-implementation-restriction-violation)
        (make-who-condition name)
        (make-message-condition "arithmetic overflow")
        (make-irritants-condition irritants))))

  (define-fx* (fxarithmetic-shift x k)
    (cond
      [($fx=? k 0) x]
      [($fx<? k 0)
        (if ($fx<=? k -32)
            (assertion-violation 'fxarithmetic-shift "shift amount more than -32" k)
            ($fxarithmetic-shift-right x ($fx- k)))]
      [else
        (let ((i (fxarithmetic-shift-left-internal x k)))
          (unless i
            (overflow-error 'fxarithmetic-shift x k))
          i)]))

  ; can be made faster : http://stackoverflow.com/q/15370250/15541
  (define-fx* (fxbit-count x)
    (cond 
      [($fx=? x 0) 0]
      [($fx<? x 0)
        ($fxnot (fxbit-count ($fxnot x)))]
      [else
        (let f ((count 0)(x x))
          (if ($fx<? 0 x)
              (f ($fx+ count ($fxand x 1))
                 ($fxarithmetic-shift-right x 1))
              count))]))

  (define-fx* (fxlength x)
    (if ($fx<? x 0)
      (fxlength ($fxnot x))
      (let f ((count 0)(x x))
        (if ($fx<? 0 x)
            (f ($fx+ count 1) ($fxarithmetic-shift-right x 1))
            count))))

  (define-fx (fxfirst-bit-set x)
    (if ($fx=? x 0)
      -1
      (let f ((count 0)(x x))
        (if (not ($fx=? 0 x))
            (if ($fx=? 1 ($fxand 1 x))
                count
                (f ($fx+ count 1)
                   ($fxarithmetic-shift-right x 1)))
            count))))

  (define-fx (fxbit-set? x k)
    (when ($fx<? k 0)
      (assertion-violation 'fxbit-set? "cannot be negative" k))
    (cond 
      [($fx=? 0 x) #f]
      [($fx>=? k 32) ($fx<? x 0)]
      [else  
        ($fx=? 1 ($fxand 1 ($fxarithmetic-shift-right x k)))]))

  (define-fx (fxnot x1)
    ($fxnot x1))

  (define-syntax define-fx-comparer 
    (lambda (x)
      (syntax-case x ()
        [(_ name)
          (with-syntax ((uname 
              (datum->syntax #'name
                (string->symbol
                  (string-append "$"
                    (symbol->string (syntax->datum #'name)))))))
            #'(define name
                (case-lambda
                  [(x1 x2)
                    (unless (fixnum? x1)
                      (assertion-violation 'name "not a fixnum" x1))
                    (unless (fixnum? x2)
                      (assertion-violation 'name "not a fixnum" x2))
                    (uname x1 x2)]
                  [(x1 x2 . rest)
                    (let f ((a x1)(b (cons x2 rest)))
                      (cond 
                        [(null? b) #t]
                        [(name a ($car b))
                          (f ($car b) ($cdr b))]
                        [else #f]))])))])))

  (define-fx-comparer fx=?)
  (define-fx-comparer fx<?)
  (define-fx-comparer fx<=?)
  (define-fx-comparer fx>?)
  (define-fx-comparer fx>=?)

  (define-syntax define-fx-bitop
    (lambda (x)
      (syntax-case x ()
        [(_ name id)
          (with-syntax ((uname 
              (datum->syntax #'name
                (string->symbol
                  (string-append "$"
                    (symbol->string (syntax->datum #'name)))))))      
            #'(define name 
                (case-lambda
                  [() id]
                  [(x)
                    (unless (fixnum? x)
                      (assertion-violation 'name "not a fixnum" x))
                    x]
                  [(x1 x2)
                    (unless (fixnum? x1)
                      (assertion-violation 'name "not a fixnum" x1))
                    (unless (fixnum? x2)
                      (assertion-violation 'name "not a fixnum" x2))
                    (uname x1 x2)]
                  [args
                    (fold-left name (name) args)])))])))

  (define-fx-bitop fxand -1)
  (define-fx-bitop fxior 0)
  (define-fx-bitop fxxor 0)

  (define-fx* (fxdiv x1 x2)
    (when ($fx=? 0 x2)
      (assertion-violation 'fxdiv "divide by zero" x1 x2))
    (when (and ($fx=? -1 x2) ($fx=? (least-fixnum) x1))
      (overflow-error 'fxdiv x1 x2))
    (cond
      [($fx=? 0 x1) 0]
      [($fx<? 0 x1) 
        ($fxdiv x1 x2)]
      [($fx<? 0 x2) 
        ($fx- ($fxdiv ($fx+ x1 1) x2) 1)]
      [else
        ($fx+ ($fxdiv ($fx+ x1 1) x2) 1)]))

  (define-fx* (fxmod x1 x2)
    ($fx- 0 ($fx- ($fx* (fxdiv* x1 x2) x2) x1)))

  (define-fx (fxdiv-and-mod x1 x2)
    (let ((d (fxdiv* x1 x2)))
      (values d ($fx- 0 ($fx- ($fx* d x2) x1))))) 

  (define-fx* (fxdiv0 x1 x2)
    (when ($fx=? 0 x2)
      (assertion-violation 'fxdiv0 "divide by zero" x1 x2))
    (when (and ($fx=? -1 x2) ($fx=? (least-fixnum) x1))
      (overflow-error 'fxdiv0 x1 x2))
    (let* ((d (fxdiv* x1 x2))
           (m ($fx- 0 ($fx- ($fx* d x2) x1)))
           (halfx2 ($fxdiv x2 2))
           (abshalfx2 (if ($fx<=? 0 halfx2)
                          halfx2
                          ($fx- 0 halfx2))))
       (cond
        [($fx<? m ($fx+ abshalfx2 ($fxand x2 1)))
          d]
        [($fx>? x2 0)
          ($fx+ d 1)]
        [else
          ($fx- d 1)])))
    
  (define-fx (fxmod0 x1 x2)
    (when ($fx=? 0 x2)
      (assertion-violation 'fxmod0 "divide by zero" x1 x2))
    (when (and ($fx=? -1 x2) ($fx=? (least-fixnum) x1))
      (overflow-error 'fxmod0 x1 x2))
    ($fx- 0 ($fx- ($fx* (fxdiv0* x1 x2) x2) x1)))
    
  (define-fx (fxdiv0-and-mod0 x1 x2)
    (let ((d (fxdiv0* x1 x2)))
      (values d ($fx- 0 ($fx- ($fx* d x2) x1)))))
      
  (define-fx* (fxpositive? r)
    ($fx<? 0 r))

  (define-fx* (fxnegative? r)
    ($fx>? 0 r))

  (define-fx* (fxzero? r)
    ($fx=? 0 r))

  (define-fx* (fxeven? n)
    ($fx=? 0 ($fxand n 1)))

  (define-fx* (fxodd? n)
    ($fx=? 1 ($fxand n 1)))

  ; TODO: improve this
  (define (fxmax a . rest)
    (unless (fixnum? a)
      (assertion-violation 'fxmax "not a fixnum" a))
    (fold-left 
      (lambda (a b) 
        (if (fx<? a b) b a))
      a 
      rest))

  ; TODO: improve this
  (define (fxmin a . rest)
    (unless (fixnum? a)
      (assertion-violation 'fxmin "not a fixnum" a))
    (fold-left 
      (lambda (a b) 
        (if (fx>? a b) b a))
      a 
      rest))

  (define-fx* (fxif fx1 fx2 fx3)
    ($fxior ($fxand fx1 fx2)
      ($fxand ($fxnot fx1) fx3)))

  (define-fx* (fxcopy-bit fx1 fx2 fx3)
    (fxif* ($fxarithmetic-shift-left 1 fx2)
      ($fxarithmetic-shift-left fx3 fx2) fx1))
  
  (define-fx* (fxbit-field fx1 fx2 fx3)
    ($fxarithmetic-shift-right 
      ($fxand fx1 ($fxnot ($fxarithmetic-shift-left -1 fx3)))
      fx2))
      
  (define-fx* (fxcopy-bit-field-nocheck to start end from)
    (fxif* 
      ($fxand 
        ($fxarithmetic-shift-left -1 start) 
        ($fxnot ($fxarithmetic-shift-left -1 end)))
      ($fxarithmetic-shift-left from start)
      to))      

  (define-fx* (fxcopy-bit-field to start end from)
    (unless ($fx<=? start end)
      (assertion-violation 'fxcopy-bit-field "start must be less than or equal end" start end)) 
    (when (or ($fx<? start 0) ($fx>=? start 32))
      (assertion-violation 'fxcopy-bit-field "start must be between 0 and 31 inclusive" start)) 
    (when (or ($fx<? end 0) ($fx>=? end 32))
      (assertion-violation 'fxcopy-bit-field "end must be between 0 and 31 inclusive" end))       
    (fxcopy-bit-field-nocheck* to start end from))

  (define-fx (fxarithmetic-shift-left fx1 fx2)
    (when (or ($fx<? fx2 0) ($fx>=? fx2 32))
      (assertion-violation 'fxarithmetic-shift-left "shift amount must be non-negative and less than 32" fx2))
    (fxarithmetic-shift* fx1 fx2))

  (define-fx (fxarithmetic-shift-right fx1 fx2)
    (when (or ($fx<? fx2 0) ($fx>=? fx2 32))
      (assertion-violation 'fxarithmetic-shift-right "shift amount must be non-negative and less than 32" fx2))
    (fxarithmetic-shift* fx1 ($fx- fx2)))

  (define-fx (fxrotate-bit-field n start end count)
    (unless ($fx<=? start end)
      (assertion-violation 'fxrotate-bit-field "start must be less than or equal end" start end))  
    (when (or ($fx<? start 0) ($fx>=? start 32))
      (assertion-violation 'fxrotate-bit-field "start must be between 0 and 31 inclusive" start)) 
    (when (or ($fx<? end 0) ($fx>=? end 32))
      (assertion-violation 'fxrotate-bit-field "end must be between 0 and 31 inclusive" end))        
    (let ((width ($fx- end start)))
      (if (fxpositive?* width)
          (let ((count ($fxmod count width))
                (field (fxbit-field* n start end)))
             (fxcopy-bit-field-nocheck* n start end 
              ($fxior 
                ($fxarithmetic-shift-left field count) 
                ($fxarithmetic-shift-right field ($fx- width count)))))
          n)))

  ;; from larceny        
  (define-fx (fxreverse-bit-field x1 start end)
    (unless ($fx<=? start end)
        (assertion-violation 'fxreverse-bit-field "start must be less than or equal end" start end))
    (when (or ($fx<? start 0) ($fx>=? start 32))
      (assertion-violation 'fxreverse-bit-field "start must be between 0 and 31 inclusive" start)) 
    (when (or ($fx<? end 0) ($fx>=? end 32))
      (assertion-violation 'fxreverse-bit-field "end must be between 0 and 31 inclusive" end))          
    (do ((width ($fx- end start) ($fx- width 1))
         (bits  (fxbit-field* x1 start end)
                ($fxarithmetic-shift-right bits 1))
         (rbits 0
                ($fxior ($fxarithmetic-shift-left rbits 1)
                        ($fxand bits 1))))
        (($fx=? width 0)
         (fxcopy-bit-field-nocheck* x1 start end rbits)))))