
(import 
  (ironscheme)
  (ironscheme clr))

(define (make-stopwatch)
  (clr-static-call System.Diagnostics.Stopwatch StartNew))
  
(define (elapsed-milliseconds sw)
  (clr-prop-get System.Timespan TotalMilliseconds 
    (clr-prop-get System.Diagnostics.Stopwatch Elapsed sw)))
    
(define (print expr iters ms)
  (display "Benchmark:  ")
  (write expr)
  (newline)
  (display "Iterations: ")
  (write iters)
  (newline)
  (display "Total ms:   ")
  (write ms)
  (newline)
  (display "Average ms: ")
  (write (/ ms iters))
  (newline)
  (newline))

    
(define-syntax bench
  (syntax-rules ()
    [(_ expr)
      (bench expr 10000000)]
    [(_ expr iters)
      (let ((sw (make-stopwatch)))
        (let loop ((i 0))
          (cond
            [(fx=? i iters)]
            [else
              expr
              (loop (fx+ i 1))]))
        (print 'expr iters (elapsed-milliseconds sw)))]))
        

(bench #t)
(bench #f)
(bench (void))
(bench (+ 1000 1000))
(bench (fx+ 1000 1000))
(bench (fl+ 1000.0 1000.0))
(bench (/ 1000 100))
(bench (fxdiv 1000 100))
(bench (fl/ 1000.0 100.0))

(let ((a 10000)(b 100))
  (bench (+ a b))
  (bench (fx+ a b))
  (bench (- a b))
  (bench (fx- a b))
  (bench (* a b))
  (bench (fx* a b)))


(bench (let ((a 10000)(b 100)) (+ a b)))
(bench (let ((a 10000)(b 100)) (fx+ a b)))
(bench (let ((a 10000)(b 100)) (- a b)))
(bench (let ((a 10000)(b 100)) (fx- a b)))
(bench (let ((a 10000)(b 100)) (* a b)))
(bench (let ((a 10000)(b 100)) (fx* a b)))


(let ()
  (let* ((a (+ 1 2))
         (b 3)
         (c a)
         (d (- c b)))
    (let* ((a c)
           (b 2)
           (c a)
           (d (- c b)))
      (* a b c d))))
 
(import (ironscheme clr)) 
(clr-using System.Runtime.InteropServices)

(define (write-int8! ptr ofs val) 
  (clr-static-call Marshal "WriteByte(IntPtr,Int32,Byte)" ptr ofs val))

(define (write-int16! ptr ofs val) 
  (clr-static-call Marshal "WriteInt16(IntPtr,Int32,Int16)" ptr ofs val))

(define (write-int32! ptr ofs val) 
  (clr-static-call Marshal "WriteInt32(IntPtr,Int32,Int32)" ptr ofs val))

(define (write-int64! ptr ofs val) 
  (clr-static-call Marshal "WriteInt64(IntPtr,Int32,Int64)" ptr ofs val))

(define (write-intptr! ptr ofs val) 
  (clr-static-call Marshal "WriteIntPtr(IntPtr,Int32,IntPtr)" ptr ofs val))
  
(define (read-int8 ptr ofs) 
  (clr-static-call Marshal "ReadByte(IntPtr,Int32)" ptr ofs))

(define (read-int16 ptr ofs) 
  (clr-static-call Marshal "ReadInt16(IntPtr,Int32)" ptr ofs))

(define (read-int32 ptr ofs) 
  (clr-static-call Marshal "ReadInt32(IntPtr,Int32)" ptr ofs))

(define (read-int64 ptr ofs) 
  (clr-static-call Marshal "ReadInt64(IntPtr,Int32)" ptr ofs))

(define (read-intptr ptr ofs) 
  (clr-static-call Marshal "ReadIntPtr(IntPtr,Int32)" ptr ofs))
  
(define (make-pointer-getter sym)
  (case sym
    [(int8 uint8)  read-int8]
    [(int16 uint16)  read-int16]
    [(int32 uint32)  read-int32]
    [(int64 uint64)  read-int64]
    [(intptr uintptr) read-intptr]
    [else (assertion-violation 'make-pointer-getter "unknown type" sym)]))
    
(define (make-pointer-setter sym)
  (case sym
    [(int8 uint8)  write-int8!]
    [(int16 uint16)  write-int16!]
    [(int32 uint32)  write-int32!]
    [(int64 uint64)  write-int64!]
    [(intptr uintptr) write!-intptr!]
    [else (assertion-violation 'make-pointer-setter "unknown type" sym)]))


(import (ironscheme ffi))   
(define dlload (pinvoke-call kernel32 LoadLibrary intptr (string)))
(define dlsym (pinvoke-call kernel32 GetProcAddress intptr (intptr string)))
(define ffitestlib (dlload "ffitest"))
(define co (dlsym ffitestlib "fnffitest"))
(define cb (dlsym ffitestlib "fnfficallback"))
(define co-sig (ffi-callout int32 (int32 int32)))
(define cb-sig (ffi-callout int32 (intptr)))
(define fnfficallback (cb-sig cb))
(define fxplus ((ffi-callback int32 (int32 int32)) 
  (lambda (x y)
    (printf "I got ~a and ~a\n" x y)
    (fx+ x y))))

(fnfficallback fxplus)

;(define malloc-sig (ffi-callout void* (uint32)))
(define malloc-sig (make-ffi-callout 'void* '(uint32)))
(define lib (dlload "msvcrt"))
(define proc (dlsym lib "malloc"))

(define malloc (malloc-sig proc))

(define mem (malloc 8))



(define ffitestlib (dlload "ffitest.dll"))
(define co (dlsym ffitestlib "fnffitest"))









