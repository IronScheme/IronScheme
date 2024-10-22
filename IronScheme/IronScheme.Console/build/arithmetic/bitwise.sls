#| License
Copyright (c) 2007-2016 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme arithmetic bitwise)
  (export
    bitwise-not
    
    bitwise-and
    bitwise-ior
    bitwise-xor
    
    bitwise-if
    
    bitwise-bit-count
    bitwise-length
    bitwise-bit-field
    
    bitwise-first-bit-set
    bitwise-bit-set?
    
    bitwise-copy-bit
    bitwise-copy-bit-field
    
    bitwise-arithmetic-shift
    bitwise-arithmetic-shift-left
    bitwise-arithmetic-shift-right
    
    bitwise-rotate-bit-field
    bitwise-reverse-bit-field)
    
  (import 
    (ironscheme clr)
    (ironscheme core)
    (except (ironscheme) 
      bitwise-if 
      bitwise-copy-bit 
      bitwise-bit-field 
      bitwise-copy-bit-field
      bitwise-arithmetic-shift-left 
      bitwise-arithmetic-shift-right
      bitwise-rotate-bit-field
      bitwise-not
      bitwise-and
      bitwise-ior
      bitwise-xor
      bitwise-bit-count
      bitwise-length
      bitwise-first-bit-set
      bitwise-bit-set?
      bitwise-reverse-bit-field
      bitwise-arithmetic-shift))
      
  (clr-using Oyster.Math)
  (clr-using IronScheme.Scripting.Math)
      
  (define ->bignum
    (typed-lambda (ei) ((Object) IntX)
      (cond
        [(bignum? ei) ei]
        [(fixnum? ei) 
          (clr-static-call IntX (Create Int32) ei)]
        [else
          (assertion-violation #f "not a exact integer" ei)])))
        
  (define (bitwise-not ei)
    (exact (clr-static-call IntX 
                            op_OnesComplement  
                            (->bignum ei))))
                            
  (define bitwise-and
    (case-lambda
      [()  -1]
      [(ei)
        ei]
      [(ei1 ei2)
        (if (and (fixnum? ei1)
                 (fixnum? ei2))
            (fxand ei1 ei2)
            (exact (clr-static-call IntX 
                                    (op_BitwiseAnd IntX IntX)
                                    (->bignum ei1)
                                    (->bignum ei2))))]
      [(ei1 ei2 . rest)
        (fold-left bitwise-and ei1 (cons ei2 rest))]))
        
  (define bitwise-ior
    (case-lambda
      [()  0]
      [(ei)
        ei]
      [(ei1 ei2)
        (if (and (fixnum? ei1)
                 (fixnum? ei2))
            (fxior ei1 ei2)      
            (exact (clr-static-call IntX 
                                    (op_BitwiseOr IntX IntX)
                                    (->bignum ei1)
                                    (->bignum ei2))))]
      [(ei1 ei2 . rest)
        (fold-left bitwise-ior ei1 (cons ei2 rest))]))

  (define bitwise-xor
    (case-lambda
      [()  0]
      [(ei)
        ei]
      [(ei1 ei2)
        (if (and (fixnum? ei1)
                 (fixnum? ei2))
            (fxxor ei1 ei2)      
            (exact (clr-static-call IntX 
                                    (op_ExclusiveOr IntX IntX)  
                                    (->bignum ei1)
                                    (->bignum ei2))))]
      [(ei1 ei2 . rest)
        (fold-left bitwise-xor ei1 (cons ei2 rest))]))

  ; can be made faster : http://stackoverflow.com/q/15370250/15541        
  (define (bitwise-bit-count ei)
    (if (fixnum? ei)
        (fxbit-count ei)
        (if (not (negative? ei))
            (let f ((c 0)(ei (->bignum ei)))
              (if (positive? ei)
                  (f (+ c (bitwise-and ei 1))
                     (bitwise-arithmetic-shift-right ei 1))
                  c))
            (bitwise-not (bitwise-bit-count (bitwise-not ei))))))
        
  (define (bitwise-length ei)
    (if (fixnum? ei)
        (fxlength ei)
        (let ((ei (->bignum ei)))
          (if (clr-static-call IntX 
                               (op_LessThan IntX IntX)
                               ei
                               (->bignum 0))
              (bitwise-length (bitwise-not ei))
              (clr-prop-get IntX BitLength ei)))))

  (define (bitwise-first-bit-set ei)
    (if (fixnum? ei)
        (fxfirst-bit-set ei)
        (let ((ei (->bignum ei)))
          (if (zero? ei)
              -1
              (let f ((c 0)(ei ei))
                (if (= 1 (bitwise-and ei 1))
                    c
                    (f (+ c 1) (bitwise-arithmetic-shift-right ei 1))))))))

  (define (bitwise-bit-set? ei k)
    (cond
      [(= -1 ei) #t]
      [(and (fixnum? ei) (< -1 k 32))
        (fxbit-set? ei k)]
      [else
        (let ((ei (->bignum ei))
              (k (->bignum k)))
          (when (negative? k)
            (assertion-violation 'bitwise-bit-set? "cannot be negative" k))
          (= 1 (bitwise-and 1 (bitwise-arithmetic-shift-right ei k))))]))
  
  (define (bitwise-if ei1 ei2 ei3)
    (bitwise-ior (bitwise-and ei1 ei2)
      (bitwise-and (bitwise-not ei1) ei3)))
      
  (define (bitwise-copy-bit ei1 ei2 ei3)
    (unless (or (zero? ei3) (= ei3 1))
      (assertion-violation 'bitwise-copy-bit "must be 0 or 1" ei3))
    (when (negative? ei2)
      (assertion-violation 'bitwise-copy-bit "cannot be negative" ei2))      
    (if (and (fixnum? ei1) (< -1 ei2 32))
        (fxcopy-bit ei1 ei2 ei3)
        (bitwise-if 
          (bitwise-arithmetic-shift-left 1 ei2)
          (bitwise-arithmetic-shift-left ei3 ei2)
          ei1)))
        
  (define (bitwise-bit-field ei1 ei2 ei3)
    (when (negative? ei2)
      (assertion-violation 'bitwise-bit-field "cannot be negative" ei2))
    (when (negative? ei3)
      (assertion-violation 'bitwise-bit-field "cannot be negative" ei3)) 
    (unless (<= ei2 ei3)
      (assertion-violation 'bitwise-bit-field "must be less than or equal" ei2 ei3))
    (if (and (fixnum? ei1) (< -1 ei3 32))
        (fxbit-field ei1 ei2 ei3)
        (bitwise-arithmetic-shift-right
          (bitwise-and ei1 (bitwise-not (bitwise-arithmetic-shift-left -1 ei3)))
          ei2)))
  
  (define (bitwise-copy-bit-field to start end from)
    (when (negative? start)
      (assertion-violation 'bitwise-copy-bit-field "cannot be negative" start))
    (when (negative? end)
      (assertion-violation 'bitwise-copy-bit-field "cannot be negative" end))   
    (unless (<= start end)
      (assertion-violation 'bitwise-copy-bit-field "must be less than or equal" start end))  
    (if (and (fixnum? to) (fixnum? from) (< -1 end 32))
        (fxcopy-bit-field to start end from)
        (bitwise-if 
          (bitwise-and 
            (bitwise-arithmetic-shift-left -1 start) 
            (bitwise-not (bitwise-arithmetic-shift-left -1 end)))
          (bitwise-arithmetic-shift-left from start)
          to)))
          
  (define (bitwise-arithmetic-shift ei k)
    (exact
      (cond 
        [(fixnum? k)
          (if (fxnegative? k)
              (cond
                [(fx=? k (least-fixnum))
                  (if (negative? ei) -1 0)]
                [(negative? ei)
                  (bitwise-not (clr-static-call IntX 
                                                op_RightShift 
                                                (->bignum (bitwise-not ei))
                                                (fx- k)))]
                [else 
                  (clr-static-call IntX 
                                   op_RightShift 
                                   (->bignum ei) 
                                   (fx- k))])
              (clr-static-call IntX 
                               op_LeftShift 
                               (->bignum ei) 
                               k))]
        [(negative? ei)
          (if (and (negative? k) (not (fixnum? k)))
              -1
              (floor (* ei (expt 2 k))))]                               
        ;; handle bignum right shifts
        [(negative? k) 
          (if (negative? ei)
              -1
              0)]
        [else ;; else &implementation-restriction
          (raise
            (condition
              (make-implementation-restriction-violation)
              (make-who-condition 'bitwise-arithmetic-shift)
              (make-message-condition "bignum left shift not supported")
              (make-irritants-condition (list ei k))))])))
                  
  (define (bitwise-arithmetic-shift-left ei1 ei2)
    (when (negative? ei2)
      (assertion-violation 'bitwise-arithmetic-shift-left "cannot be negative" ei2))
    (bitwise-arithmetic-shift ei1 ei2))            
    
  (define (bitwise-arithmetic-shift-right ei1 ei2)
    (when (negative? ei2)
      (assertion-violation 'bitwise-arithmetic-shift-right "cannot be negative" ei2))
    (bitwise-arithmetic-shift ei1 (- ei2)))            
    
  (define (bitwise-rotate-bit-field n start end count)
    (when (negative? start)
      (assertion-violation 'bitwise-rotate-bit-field "cannot be negative" start))
    (when (negative? end)
      (assertion-violation 'bitwise-rotate-bit-field "cannot be negative" end))
    (when (negative? count)
      (assertion-violation 'bitwise-rotate-bit-field "cannot be negative" count))      
    (unless (<= start end)
      (assertion-violation 'bitwise-rotate-bit-field "start must be less than or equal to end" start end))
    (let ((width (- end start)))
      (if (positive? width)
          (let ((count (mod count width))
                (field (bitwise-bit-field n start end)))
             (bitwise-copy-bit-field n start end 
              (bitwise-ior 
                (bitwise-arithmetic-shift-left field count) 
                (bitwise-arithmetic-shift-right field (- width count)))))
          n)))
        
  (define (bitwise-reverse-bit-field x1 start end)
    (when (negative? start)
      (assertion-violation 'bitwise-reverse-bit-field "cannot be negative" start))
    (when (negative? end)
      (assertion-violation 'bitwise-reverse-bit-field "cannot be negative" end))  
    (unless (<= start end)
      (assertion-violation 'bitwise-reverse-bit-field "start must be less than or equal to end" start end))
    (do ((width (- end start) (- width 1))
         (bits  (bitwise-bit-field x1 start end)
                (bitwise-arithmetic-shift-right bits 1))
         (rbits 0
                (bitwise-ior (bitwise-arithmetic-shift-left rbits 1)
                             (bitwise-and bits 1))))
        ((= width 0)
         (bitwise-copy-bit-field x1 start end rbits)))))
