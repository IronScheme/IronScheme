#| License
Copyright (c) 2007-2015 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme typed fixnums)
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
    (ironscheme clr)
    (ironscheme typed)
    (ironscheme unsafe)
    (ironscheme syntax utils)
    (ironscheme syntax shorthand)
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
        
  (define-syntax define-fx
    (lambda (x)
      (define (pred? name)
        (clr-call String EndsWith (symbol->string (syntax->datum name)) "?"))
      (syntax-case x ()
        [(_ (name formals ...) body body* ...)
          (with-syntax (((type ...) (map (lambda (x) (datum->syntax x 'Int32)) #'(formals ...)))
                        (ret-type (datum->syntax #'name (if (pred? #'name) 'Boolean 'Int32))))
            #'(define: (name (formals : type) ... -> ret-type)
                body body* ...))])))

  (define-fx (fixnum-width) 32)
  
  (define-fx (greatest-fixnum)  #x7fffffff)
  (define-fx (least-fixnum)     #x-80000000)      
      
  (define-fx (fxadd1 x)
    ($fx+ x 1))

  (define-fx (fxsub1 x)
    ($fx- x 1))

  (define-fx (fx+ x1 x2)
    ($fx+ x1 x2))

  (define-fx (fx* x1 x2)
    ($fx* x1 x2))

  (define fx-
    (case-lambda:
      [((x1 : Int32) -> Int32)
        ($fx- x1)]
      [((x1 : Int32) (x2 : Int32) -> Int32)
        ($fx- x1 x2)]))

  (define-fx (fxarithmetic-shift x k)
    (cond
      [($fx<? k 0)
        ($fxarithmetic-shift-right x ($fx- k))]
      [else
        ($fxarithmetic-shift-left x k)]))

  (define-fx (fxbit-count x)
    (cond 
      [($fx<? x 0)
        ($fxnot (fxbit-count ($fxnot x)))]
      [else
        (let: f ((count 0)(x x) -> Int32)
          (if (not ($fx=? 0 x))
              (f ($fx+ count 1)
                 ($fxand x ($fx- x 1)))
              count))]))

  (define-fx (fxlength x)
    (if ($fx<? x 0)
      (fxlength ($fxnot x))
      (let: f ((count 0)(x x) -> Int32)
        (if ($fx<? 0 x)
            (f ($fx+ count 1) ($fxarithmetic-shift-right x 1))
            count))))

  (define-fx (fxfirst-bit-set x)
    (if ($fx=? x 0)
      -1
      (let: f ((count 0)(x x) -> Int32)
        (if (not ($fx=? 0 x))
            (if ($fx=? 1 ($fxand 1 x))
                count
                (f ($fx+ count 1)
                   ($fxarithmetic-shift-right x 1)))
            count))))

  (define-fx (fxbit-set? x k)
    ($fx=? 1 ($fxand 1 ($fxarithmetic-shift-right x k))))

  (define-fx (fxnot x1)
    ($fxnot x1))

  (define-syntax-case (define-fx-comparer name)
    (with-syntax ((uname (syntax-format "$~a" #'name #'name)))
      #'(define name
          (case-lambda:
            [((x1 : Int32) (x2 : Int32) -> Boolean)
              (uname x1 x2)]
            ;; this is bit hard for now...
            #;[(x1 x2 . rest)
              (let f ((a x1)(b (cons x2 rest)))
                (cond 
                  [(null? b) #t]
                  [(name a ($car b))
                    (f ($car b) ($cdr b))]
                  [else #f]))]))))

  (define-fx-comparer fx=?)
  (define-fx-comparer fx<?)
  (define-fx-comparer fx<=?)
  (define-fx-comparer fx>?)
  (define-fx-comparer fx>=?)

  (define-syntax-case (define-fx-bitop name id)
    (with-syntax ((uname (syntax-format "$~a" #'name #'name)))
      #'(define name 
          (case-lambda:
            [(-> Int32) id]
            [((x : Int32) -> Int32) x]
            [((x1 : Int32) (x2 : Int32) -> Int32)
              (uname x1 x2)]))))

  (define-fx-bitop fxand -1)
  (define-fx-bitop fxior 0)
  (define-fx-bitop fxxor 0)

  (define-fx (fxdiv x1 x2)
    (cond
      [($fx=? 0 x1) 0]
      [($fx<? 0 x1) 
        ($fxdiv x1 x2)]
      [($fx<? 0 x2) 
        ($fx- ($fxdiv ($fx+ x1 1) x2) 1)]
      [else
        ($fx+ ($fxdiv ($fx+ x1 1) x2) 1)]))

  (define-fx (fxmod x1 x2)
    ($fx- 0 ($fx- ($fx* (fxdiv x1 x2) x2) x1)))

  ;; untyped
  (define (fxdiv-and-mod x1 x2)
    (import (rnrs))
    (let ((d (fxdiv x1 x2)))
      (values d ($fx- 0 ($fx- ($fx* d x2) x1))))) 

  (define-fx (fxdiv0 x1 x2)
    (let* ((d (fxdiv x1 x2))
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
    ($fx- 0 ($fx- ($fx* (fxdiv0 x1 x2) x2) x1)))
  
  ;; untyped
  (define (fxdiv0-and-mod0 x1 x2)
    (let ((d (fxdiv0 x1 x2)))
      (values d ($fx- 0 ($fx- ($fx* d x2) x1)))))
      
  (define-fx (fxpositive? r)
    ($fx<? 0 r))

  (define-fx (fxnegative? r)
    ($fx>? 0 r))

  (define-fx (fxzero? r)
    ($fx=? 0 r))

  (define-fx (fxeven? n)
    ($fx=? 0 ($fxand n 1)))

  (define-fx (fxodd? n)
    ($fx=? 1 ($fxand n 1)))

  (define-fx (fxmax a b)
    (if ($fx<? a b) b a))

  (define-fx (fxmin a b)
    (if ($fx>? a b) b a))

  (define-fx (fxif fx1 fx2 fx3)
    ($fxior ($fxand fx1 fx2)
      ($fxand ($fxnot fx1) fx3)))

  (define-fx (fxcopy-bit fx1 fx2 fx3)
    (fxif ($fxarithmetic-shift-left 1 fx2)
      ($fxarithmetic-shift-left fx3 fx2) fx1))
  
  (define-fx (fxbit-field fx1 fx2 fx3)
    ($fxarithmetic-shift-right 
      ($fxand fx1 ($fxnot ($fxarithmetic-shift-left -1 fx3)))
      fx2))

  (define-fx (fxcopy-bit-field to start end from)
    (fxif 
      ($fxand 
        ($fxarithmetic-shift-left -1 start) 
        ($fxnot ($fxarithmetic-shift-left -1 end)))
      ($fxarithmetic-shift-left from start)
      to))

  (define-fx (fxarithmetic-shift-left fx1 fx2)
    ($fxarithmetic-shift-left fx1 fx2))

  (define-fx (fxarithmetic-shift-right fx1 fx2)
    ($fxarithmetic-shift-right fx1 fx2))

  (define-fx (fxrotate-bit-field n start end count)
    (let* ((width ($fx- end start))
           (field1 (fxbit-field n start ($fx- end count)))
           (field2 (fxbit-field n start end)))
       (fxcopy-bit-field n start end 
        ($fxior 
          ($fxarithmetic-shift-left field1 count) 
          ($fxarithmetic-shift-right field2 ($fx- width count))))))

  ;; from larceny        
  (define-fx (fxreverse-bit-field x1 start end)
    (do ((width ($fx- end start) ($fx- width 1))
         (bits  (fxbit-field x1 start end)
                ($fxarithmetic-shift-right bits 1))
         (rbits 0
                ($fxior ($fxarithmetic-shift-left rbits 1)
                        ($fxand bits 1))))
        (($fx=? width 0)
         (fxcopy-bit-field x1 start end rbits))))
         
         
  #;(clr-static-call System.Diagnostics.Debugger Break))