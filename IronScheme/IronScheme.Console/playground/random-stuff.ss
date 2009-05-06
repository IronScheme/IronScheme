
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







(library-letrec* (srfi :42 eager-comprehensions)
  ((g$error$54$rW3Dr
     g$error$2606$rW3Dr
     (case-lambda
       (g$args$2437$rW3Dr
        ((case-lambda
           ((g$ER:error-who$2440$rW3Dr
              g$(library (srfi :42 eager-comprehensions))$2441$rW3Dr)
            ((case-lambda
               ((g$swap$2442$rW3Dr)
                (dynamic-wind
                  g$swap$2442$rW3Dr
                  (case-lambda
                    (() (apply g$error$4$rW3Dr g$args$2437$rW3Dr)))
                  g$swap$2442$rW3Dr)))
             (case-lambda
               (()
                ((case-lambda
                   ((g$t$2443$rW3Dr)
                    (begin
                      (g$ER:error-who$2440$rW3Dr
                        g$(library (srfi :42 eager-comprehensions))$2441$rW3Dr)
                      (set! g$(library (srfi :42 eager-comprehensions))$2441$rW3Dr
                        g$t$2443$rW3Dr))))
                 (g$ER:error-who$2440$rW3Dr)))))))
         g$error-who$3$rW3Dr
         '"(library (srfi :42 eager-comprehensions))"))))
   (g$ec-:vector-filter$1024$rW3Dr
     g$ec-:vector-filter$2607$rW3Dr
     (case-lambda
       ((g$vecs$2444$rW3Dr)
        (if (null? g$vecs$2444$rW3Dr)
          '()
          (if (g$zero?$17011$r8tjl
                (g$vector-length$16452$r8tjl (car g$vecs$2444$rW3Dr)))
            (g$ec-:vector-filter$1024$rW3Dr (cdr g$vecs$2444$rW3Dr))
            (cons
              (car g$vecs$2444$rW3Dr)
              (g$ec-:vector-filter$1024$rW3Dr
                (cdr g$vecs$2444$rW3Dr))))))))
   (g$dispatch-union$1629$rW3Dr
     g$dispatch-union$2608$rW3Dr
     (case-lambda
       ((g$d1$2445$rW3Dr g$d2$2446$rW3Dr)
        (case-lambda
          ((g$args$2447$rW3Dr)
           ((case-lambda
              ((g$g1$2448$rW3Dr g$g2$2449$rW3Dr)
               (if g$g1$2448$rW3Dr
                 (if g$g2$2449$rW3Dr
                   (if (null? g$args$2447$rW3Dr)
                     (g$append$16000$r8tjl
                       (if (g$list?$15989$r8tjl g$g1$2448$rW3Dr)
                         g$g1$2448$rW3Dr
                         (list g$g1$2448$rW3Dr))
                       (if (g$list?$15989$r8tjl g$g2$2449$rW3Dr)
                         g$g2$2449$rW3Dr
                         (list g$g2$2449$rW3Dr)))
                     (g$error$54$rW3Dr
                       '"dispatching conflict"
                       g$args$2447$rW3Dr
                       (g$d1$2445$rW3Dr '())
                       (g$d2$2446$rW3Dr '())))
                   g$g1$2448$rW3Dr)
                 (if g$g2$2449$rW3Dr g$g2$2449$rW3Dr '#f))))
            (g$d1$2445$rW3Dr g$args$2447$rW3Dr)
            (g$d2$2446$rW3Dr g$args$2447$rW3Dr)))))))
   (g$make-initial-:-dispatch$1630$rW3Dr
     g$make-initial-:-dispatch$2609$rW3Dr
     (case-lambda
       (()
        (case-lambda
          ((g$args$2450$rW3Dr)
           ((case-lambda
              ((g$t$2451$rW3Dr)
               (if (eqv? g$t$2451$rW3Dr '0)
                 'SRFI42
                 (if (eqv? g$t$2451$rW3Dr '1)
                   ((case-lambda
                      ((g$a1$2452$rW3Dr)
                       (if (g$list?$15989$r8tjl g$a1$2452$rW3Dr)
                         ((case-lambda
                            ((g$t$2453$rW3Dr g$ne2$2454$rW3Dr)
                             ((case-lambda
                                ((g$var$2455$rW3Dr)
                                 (case-lambda
                                   ((g$empty$2456$rW3Dr)
                                    (if (if (not (null? g$t$2453$rW3Dr))
                                          (begin '#f g$ne2$2454$rW3Dr)
                                          '#f)
                                      (begin
                                        (set! g$var$2455$rW3Dr
                                          (car g$t$2453$rW3Dr))
                                        ((case-lambda
                                           ((g$value$2457$rW3Dr)
                                            (begin
                                              (set! g$t$2453$rW3Dr
                                                (cdr g$t$2453$rW3Dr))
                                              g$value$2457$rW3Dr)))
                                         g$var$2455$rW3Dr))
                                      g$empty$2456$rW3Dr)))))
                              '#f)))
                          g$a1$2452$rW3Dr
                          '#t)
                         (if (g$string?$16416$r8tjl g$a1$2452$rW3Dr)
                           ((case-lambda
                              ((g$str$2458$rW3Dr g$len$2459$rW3Dr)
                               (begin
                                 (set! g$len$2459$rW3Dr
                                   (g$string-length$16432$r8tjl
                                     g$str$2458$rW3Dr))
                                 ((case-lambda
                                    ((g$i$2460$rW3Dr g$ne2$2461$rW3Dr)
                                     ((case-lambda
                                        ((g$var$2462$rW3Dr)
                                         (case-lambda
                                           ((g$empty$2463$rW3Dr)
                                            (if (if (g$<$17021$r8tjl
                                                      g$i$2460$rW3Dr
                                                      g$len$2459$rW3Dr)
                                                  (begin
                                                    '#f
                                                    g$ne2$2461$rW3Dr)
                                                  '#f)
                                              (begin
                                                (set! g$var$2462$rW3Dr
                                                  (g$string-ref$16429$r8tjl
                                                    g$str$2458$rW3Dr
                                                    g$i$2460$rW3Dr))
                                                ((case-lambda
                                                   ((g$value$2464$rW3Dr)
                                                    (begin
                                                      (set! g$i$2460$rW3Dr
                                                        (+ g$i$2460$rW3Dr
                                                           '1))
                                                      g$value$2464$rW3Dr)))
                                                 g$var$2462$rW3Dr))
                                              g$empty$2463$rW3Dr)))))
                                      '#f)))
                                  '0
                                  '#t))))
                            g$a1$2452$rW3Dr
                            '0)
                           (if (g$vector?$16418$r8tjl g$a1$2452$rW3Dr)
                             ((case-lambda
                                ((g$vec$2465$rW3Dr g$len$2466$rW3Dr)
                                 (begin
                                   (set! g$len$2466$rW3Dr
                                     (g$vector-length$16452$r8tjl
                                       g$vec$2465$rW3Dr))
                                   ((case-lambda
                                      ((g$i$2467$rW3Dr g$ne2$2468$rW3Dr)
                                       ((case-lambda
                                          ((g$var$2469$rW3Dr)
                                           (case-lambda
                                             ((g$empty$2470$rW3Dr)
                                              (if (if (g$<$17021$r8tjl
                                                        g$i$2467$rW3Dr
                                                        g$len$2466$rW3Dr)
                                                    (begin
                                                      '#f
                                                      g$ne2$2468$rW3Dr)
                                                    '#f)
                                                (begin
                                                  (set! g$var$2469$rW3Dr
                                                    (g$vector-ref$16449$r8tjl
                                                      g$vec$2465$rW3Dr
                                                      g$i$2467$rW3Dr))
                                                  ((case-lambda
                                                     ((g$value$2471$rW3Dr)
                                                      (begin
                                                        (set! g$i$2467$rW3Dr
                                                          (+ g$i$2467$rW3Dr
                                                             '1))
                                                        g$value$2471$rW3Dr)))
                                                   g$var$2469$rW3Dr))
                                                g$empty$2470$rW3Dr)))))
                                        '#f)))
                                    '0
                                    '#t))))
                              g$a1$2452$rW3Dr
                              '0)
                             (if (if (g$integer?$17007$r8tjl
                                       g$a1$2452$rW3Dr)
                                   (begin
                                     '#f
                                     (g$exact?$17002$r8tjl
                                       g$a1$2452$rW3Dr))
                                   '#f)
                               ((case-lambda
                                  ((g$b$2472$rW3Dr)
                                   (begin
                                     (if (not (if (g$integer?$17007$r8tjl
                                                    g$b$2472$rW3Dr)
                                                (begin
                                                  '#f
                                                  (g$exact?$17002$r8tjl
                                                    g$b$2472$rW3Dr))
                                                '#f))
                                       (g$error$54$rW3Dr
                                         '"arguments of :range are not exact integer "
                                         '"(use :real-range?)"
                                         '0
                                         g$b$2472$rW3Dr
                                         '1)
                                       (void))
                                     ((case-lambda
                                        ((g$var$2473$rW3Dr
                                           g$ne2$2474$rW3Dr)
                                         (case-lambda
                                           ((g$empty$2475$rW3Dr)
                                            (if (if (g$<$17021$r8tjl
                                                      g$var$2473$rW3Dr
                                                      g$b$2472$rW3Dr)
                                                  (begin
                                                    '#f
                                                    g$ne2$2474$rW3Dr)
                                                  '#f)
                                              ((case-lambda
                                                 ((g$value$2476$rW3Dr)
                                                  (begin
                                                    (set! g$var$2473$rW3Dr
                                                      (+ g$var$2473$rW3Dr
                                                         '1))
                                                    g$value$2476$rW3Dr)))
                                               g$var$2473$rW3Dr)
                                              g$empty$2475$rW3Dr)))))
                                      '0
                                      '#t))))
                                g$a1$2452$rW3Dr)
                               (if (g$real?$17005$r8tjl g$a1$2452$rW3Dr)
                                 ((case-lambda
                                    ((g$a$2477$rW3Dr
                                       g$b$2478$rW3Dr
                                       g$s$2479$rW3Dr
                                       g$istop$2480$rW3Dr)
                                     (begin
                                       (if (not (if (g$real?$17005$r8tjl
                                                      g$a$2477$rW3Dr)
                                                  (if (g$real?$17005$r8tjl
                                                        g$b$2478$rW3Dr)
                                                    (begin
                                                      '#f
                                                      (g$real?$17005$r8tjl
                                                        g$s$2479$rW3Dr))
                                                    '#f)
                                                  '#f))
                                         (g$error$54$rW3Dr
                                           '"arguments of :real-range are not real"
                                           g$a$2477$rW3Dr
                                           g$b$2478$rW3Dr
                                           g$s$2479$rW3Dr)
                                         (void))
                                       (if (if (g$exact?$17002$r8tjl
                                                 g$a$2477$rW3Dr)
                                             (begin
                                               '#f
                                               ((case-lambda
                                                  ((g$t$2481$rW3Dr)
                                                   (if g$t$2481$rW3Dr
                                                     g$t$2481$rW3Dr
                                                     (begin
                                                       '#f
                                                       (not (g$exact?$17002$r8tjl
                                                              g$s$2479$rW3Dr))))))
                                                (not (g$exact?$17002$r8tjl
                                                       g$b$2478$rW3Dr))))
                                             '#f)
                                         (set! g$a$2477$rW3Dr
                                           (g$exact->inexact$17746$r8tjl
                                             g$a$2477$rW3Dr))
                                         (void))
                                       (set! g$istop$2480$rW3Dr
                                         (/ (- g$b$2478$rW3Dr
                                               g$a$2477$rW3Dr)
                                            g$s$2479$rW3Dr))
                                       ((case-lambda
                                          ((g$i$2482$rW3Dr
                                             g$ne2$2483$rW3Dr)
                                           ((case-lambda
                                              ((g$var$2484$rW3Dr)
                                               (case-lambda
                                                 ((g$empty$2485$rW3Dr)
                                                  (if (if (g$<$17021$r8tjl
                                                            g$i$2482$rW3Dr
                                                            g$istop$2480$rW3Dr)
                                                        (begin
                                                          '#f
                                                          g$ne2$2483$rW3Dr)
                                                        '#f)
                                                    (begin
                                                      (set! g$var$2484$rW3Dr
                                                        (+ g$a$2477$rW3Dr
                                                           (* g$s$2479$rW3Dr
                                                              g$i$2482$rW3Dr)))
                                                      ((case-lambda
                                                         ((g$value$2486$rW3Dr)
                                                          (begin
                                                            (set! g$i$2482$rW3Dr
                                                              (+ g$i$2482$rW3Dr
                                                                 '1))
                                                            g$value$2486$rW3Dr)))
                                                       g$var$2484$rW3Dr))
                                                    g$empty$2485$rW3Dr)))))
                                            '#f)))
                                        '0
                                        '#t))))
                                  '0
                                  g$a1$2452$rW3Dr
                                  '1
                                  '0)
                                 (if (input-port? g$a1$2452$rW3Dr)
                                   ((case-lambda
                                      ((g$port$2487$rW3Dr
                                         g$read-proc$2488$rW3Dr)
                                       ((case-lambda
                                          ((g$var$2489$rW3Dr
                                             g$ne2$2490$rW3Dr)
                                           (case-lambda
                                             ((g$empty$2491$rW3Dr)
                                              (if (if (not (eof-object?
                                                             g$var$2489$rW3Dr))
                                                    (begin
                                                      '#f
                                                      g$ne2$2490$rW3Dr)
                                                    '#f)
                                                ((case-lambda
                                                   ((g$value$2492$rW3Dr)
                                                    (begin
                                                      (set! g$var$2489$rW3Dr
                                                        (g$read-proc$2488$rW3Dr
                                                          g$port$2487$rW3Dr))
                                                      g$value$2492$rW3Dr)))
                                                 g$var$2489$rW3Dr)
                                                g$empty$2491$rW3Dr)))))
                                        (g$read-proc$2488$rW3Dr
                                          g$port$2487$rW3Dr)
                                        '#t)))
                                    g$a1$2452$rW3Dr
                                    read)
                                   ((case-lambda
                                      (() (begin '#f '#f))))))))))))
                    (car g$args$2450$rW3Dr))
                   (if (eqv? g$t$2451$rW3Dr '2)
                     ((case-lambda
                        ((g$a1$2493$rW3Dr g$a2$2494$rW3Dr)
                         (if (if (g$list?$15989$r8tjl g$a1$2493$rW3Dr)
                               (begin
                                 '#f
                                 (g$list?$15989$r8tjl g$a2$2494$rW3Dr))
                               '#f)
                           ((case-lambda
                              ((g$t$2495$rW3Dr g$ne2$2496$rW3Dr)
                               ((case-lambda
                                  ((g$var$2497$rW3Dr)
                                   (case-lambda
                                     ((g$empty$2498$rW3Dr)
                                      (if (if (not (null?
                                                     g$t$2495$rW3Dr))
                                            (begin '#f g$ne2$2496$rW3Dr)
                                            '#f)
                                        (begin
                                          (set! g$var$2497$rW3Dr
                                            (car g$t$2495$rW3Dr))
                                          ((case-lambda
                                             ((g$value$2499$rW3Dr)
                                              (begin
                                                (set! g$t$2495$rW3Dr
                                                  (cdr g$t$2495$rW3Dr))
                                                g$value$2499$rW3Dr)))
                                           g$var$2497$rW3Dr))
                                        g$empty$2498$rW3Dr)))))
                                '#f)))
                            (g$append$16000$r8tjl
                              g$a1$2493$rW3Dr
                              g$a2$2494$rW3Dr)
                            '#t)
                           (if (if (g$string?$16416$r8tjl
                                     g$a1$2493$rW3Dr)
                                 (begin
                                   '#f
                                   (g$string?$16416$r8tjl
                                     g$a2$2494$rW3Dr))
                                 '#f)
                             ((case-lambda
                                ((g$str$2500$rW3Dr g$len$2501$rW3Dr)
                                 (begin
                                   (set! g$len$2501$rW3Dr
                                     (g$string-length$16432$r8tjl
                                       g$str$2500$rW3Dr))
                                   ((case-lambda
                                      ((g$i$2502$rW3Dr g$ne2$2503$rW3Dr)
                                       ((case-lambda
                                          ((g$var$2504$rW3Dr)
                                           (case-lambda
                                             ((g$empty$2505$rW3Dr)
                                              (if (if (g$<$17021$r8tjl
                                                        g$i$2502$rW3Dr
                                                        g$len$2501$rW3Dr)
                                                    (begin
                                                      '#f
                                                      g$ne2$2503$rW3Dr)
                                                    '#f)
                                                (begin
                                                  (set! g$var$2504$rW3Dr
                                                    (g$string-ref$16429$r8tjl
                                                      g$str$2500$rW3Dr
                                                      g$i$2502$rW3Dr))
                                                  ((case-lambda
                                                     ((g$value$2506$rW3Dr)
                                                      (begin
                                                        (set! g$i$2502$rW3Dr
                                                          (+ g$i$2502$rW3Dr
                                                             '1))
                                                        g$value$2506$rW3Dr)))
                                                   g$var$2504$rW3Dr))
                                                g$empty$2505$rW3Dr)))))
                                        '#f)))
                                    '0
                                    '#t))))
                              (g$string-append$16438$r8tjl
                                g$a1$2493$rW3Dr
                                g$a2$2494$rW3Dr)
                              '0)
                             (if (if (g$vector?$16418$r8tjl
                                       g$a1$2493$rW3Dr)
                                   (begin
                                     '#f
                                     (g$vector?$16418$r8tjl
                                       g$a2$2494$rW3Dr))
                                   '#f)
                               ((case-lambda
                                  ((g$vec$2507$rW3Dr
                                     g$len$2508$rW3Dr
                                     g$vecs$2509$rW3Dr)
                                   ((case-lambda
                                      ((g$k$2510$rW3Dr g$ne2$2511$rW3Dr)
                                       ((case-lambda
                                          ((g$var$2512$rW3Dr)
                                           (case-lambda
                                             ((g$empty$2513$rW3Dr)
                                              (if (if (if (g$<$17021$r8tjl
                                                            g$k$2510$rW3Dr
                                                            g$len$2508$rW3Dr)
                                                        '#t
                                                        (if (null?
                                                              g$vecs$2509$rW3Dr)
                                                          '#f
                                                          (begin
                                                            (set! g$vec$2507$rW3Dr
                                                              (car g$vecs$2509$rW3Dr))
                                                            (set! g$vecs$2509$rW3Dr
                                                              (cdr g$vecs$2509$rW3Dr))
                                                            (set! g$len$2508$rW3Dr
                                                              (g$vector-length$16452$r8tjl
                                                                g$vec$2507$rW3Dr))
                                                            (set! g$k$2510$rW3Dr
                                                              '0)
                                                            '#t)))
                                                    (begin
                                                      '#f
                                                      g$ne2$2511$rW3Dr)
                                                    '#f)
                                                (begin
                                                  (set! g$var$2512$rW3Dr
                                                    (g$vector-ref$16449$r8tjl
                                                      g$vec$2507$rW3Dr
                                                      g$k$2510$rW3Dr))
                                                  ((case-lambda
                                                     ((g$value$2514$rW3Dr)
                                                      (begin
                                                        (set! g$k$2510$rW3Dr
                                                          (+ g$k$2510$rW3Dr
                                                             '1))
                                                        g$value$2514$rW3Dr)))
                                                   g$var$2512$rW3Dr))
                                                g$empty$2513$rW3Dr)))))
                                        '#f)))
                                    '0
                                    '#t)))
                                '#f
                                '0
                                (g$ec-:vector-filter$1024$rW3Dr
                                  (list
                                    g$a1$2493$rW3Dr
                                    g$a2$2494$rW3Dr)))
                               (if (if (g$integer?$17007$r8tjl
                                         g$a1$2493$rW3Dr)
                                     (if (g$exact?$17002$r8tjl
                                           g$a1$2493$rW3Dr)
                                       (if (g$integer?$17007$r8tjl
                                             g$a2$2494$rW3Dr)
                                         (begin
                                           '#f
                                           (g$exact?$17002$r8tjl
                                             g$a2$2494$rW3Dr))
                                         '#f)
                                       '#f)
                                     '#f)
                                 ((case-lambda
                                    ((g$a$2515$rW3Dr g$b$2516$rW3Dr)
                                     (begin
                                       (if (not (if (g$integer?$17007$r8tjl
                                                      g$a$2515$rW3Dr)
                                                  (if (g$exact?$17002$r8tjl
                                                        g$a$2515$rW3Dr)
                                                    (if (g$integer?$17007$r8tjl
                                                          g$b$2516$rW3Dr)
                                                      (begin
                                                        '#f
                                                        (g$exact?$17002$r8tjl
                                                          g$b$2516$rW3Dr))
                                                      '#f)
                                                    '#f)
                                                  '#f))
                                         (g$error$54$rW3Dr
                                           '"arguments of :range are not exact integer "
                                           '"(use :real-range?)"
                                           g$a$2515$rW3Dr
                                           g$b$2516$rW3Dr
                                           '1)
                                         (void))
                                       ((case-lambda
                                          ((g$var$2517$rW3Dr
                                             g$ne2$2518$rW3Dr)
                                           (case-lambda
                                             ((g$empty$2519$rW3Dr)
                                              (if (if (g$<$17021$r8tjl
                                                        g$var$2517$rW3Dr
                                                        g$b$2516$rW3Dr)
                                                    (begin
                                                      '#f
                                                      g$ne2$2518$rW3Dr)
                                                    '#f)
                                                ((case-lambda
                                                   ((g$value$2520$rW3Dr)
                                                    (begin
                                                      (set! g$var$2517$rW3Dr
                                                        (+ g$var$2517$rW3Dr
                                                           '1))
                                                      g$value$2520$rW3Dr)))
                                                 g$var$2517$rW3Dr)
                                                g$empty$2519$rW3Dr)))))
                                        g$a$2515$rW3Dr
                                        '#t))))
                                  g$a1$2493$rW3Dr
                                  g$a2$2494$rW3Dr)
                                 (if (if (g$real?$17005$r8tjl
                                           g$a1$2493$rW3Dr)
                                       (begin
                                         '#f
                                         (g$real?$17005$r8tjl
                                           g$a2$2494$rW3Dr))
                                       '#f)
                                   ((case-lambda
                                      ((g$a$2521$rW3Dr
                                         g$b$2522$rW3Dr
                                         g$s$2523$rW3Dr
                                         g$istop$2524$rW3Dr)
                                       (begin
                                         (if (not (if (g$real?$17005$r8tjl
                                                        g$a$2521$rW3Dr)
                                                    (if (g$real?$17005$r8tjl
                                                          g$b$2522$rW3Dr)
                                                      (begin
                                                        '#f
                                                        (g$real?$17005$r8tjl
                                                          g$s$2523$rW3Dr))
                                                      '#f)
                                                    '#f))
                                           (g$error$54$rW3Dr
                                             '"arguments of :real-range are not real"
                                             g$a$2521$rW3Dr
                                             g$b$2522$rW3Dr
                                             g$s$2523$rW3Dr)
                                           (void))
                                         (if (if (g$exact?$17002$r8tjl
                                                   g$a$2521$rW3Dr)
                                               (begin
                                                 '#f
                                                 ((case-lambda
                                                    ((g$t$2525$rW3Dr)
                                                     (if g$t$2525$rW3Dr
                                                       g$t$2525$rW3Dr
                                                       (begin
                                                         '#f
                                                         (not (g$exact?$17002$r8tjl
                                                                g$s$2523$rW3Dr))))))
                                                  (not (g$exact?$17002$r8tjl
                                                         g$b$2522$rW3Dr))))
                                               '#f)
                                           (set! g$a$2521$rW3Dr
                                             (g$exact->inexact$17746$r8tjl
                                               g$a$2521$rW3Dr))
                                           (void))
                                         (set! g$istop$2524$rW3Dr
                                           (/ (- g$b$2522$rW3Dr
                                                 g$a$2521$rW3Dr)
                                              g$s$2523$rW3Dr))
                                         ((case-lambda
                                            ((g$i$2526$rW3Dr
                                               g$ne2$2527$rW3Dr)
                                             ((case-lambda
                                                ((g$var$2528$rW3Dr)
                                                 (case-lambda
                                                   ((g$empty$2529$rW3Dr)
                                                    (if (if (g$<$17021$r8tjl
                                                              g$i$2526$rW3Dr
                                                              g$istop$2524$rW3Dr)
                                                          (begin
                                                            '#f
                                                            g$ne2$2527$rW3Dr)
                                                          '#f)
                                                      (begin
                                                        (set! g$var$2528$rW3Dr
                                                          (+ g$a$2521$rW3Dr
                                                             (* g$s$2523$rW3Dr
                                                                g$i$2526$rW3Dr)))
                                                        ((case-lambda
                                                           ((g$value$2530$rW3Dr)
                                                            (begin
                                                              (set! g$i$2526$rW3Dr
                                                                (+ g$i$2526$rW3Dr
                                                                   '1))
                                                              g$value$2530$rW3Dr)))
                                                         g$var$2528$rW3Dr))
                                                      g$empty$2529$rW3Dr)))))
                                              '#f)))
                                          '0
                                          '#t))))
                                    g$a1$2493$rW3Dr
                                    g$a2$2494$rW3Dr
                                    '1
                                    '0)
                                   (if (if (g$char?$16417$r8tjl
                                             g$a1$2493$rW3Dr)
                                         (begin
                                           '#f
                                           (g$char?$16417$r8tjl
                                             g$a2$2494$rW3Dr))
                                         '#f)
                                     ((case-lambda
                                        ((g$imax$2531$rW3Dr)
                                         ((case-lambda
                                            ((g$i$2532$rW3Dr
                                               g$ne2$2533$rW3Dr)
                                             ((case-lambda
                                                ((g$var$2534$rW3Dr)
                                                 (case-lambda
                                                   ((g$empty$2535$rW3Dr)
                                                    (if (if (g$<=$17022$r8tjl
                                                              g$i$2532$rW3Dr
                                                              g$imax$2531$rW3Dr)
                                                          (begin
                                                            '#f
                                                            g$ne2$2533$rW3Dr)
                                                          '#f)
                                                      (begin
                                                        (set! g$var$2534$rW3Dr
                                                          (g$integer->char$16427$r8tjl
                                                            g$i$2532$rW3Dr))
                                                        ((case-lambda
                                                           ((g$value$2536$rW3Dr)
                                                            (begin
                                                              (set! g$i$2532$rW3Dr
                                                                (+ g$i$2532$rW3Dr
                                                                   '1))
                                                              g$value$2536$rW3Dr)))
                                                         g$var$2534$rW3Dr))
                                                      g$empty$2535$rW3Dr)))))
                                              '#f)))
                                          (g$char->integer$16426$r8tjl
                                            g$a1$2493$rW3Dr)
                                          '#t)))
                                      (g$char->integer$16426$r8tjl
                                        g$a2$2494$rW3Dr))
                                     (if (if (input-port?
                                               g$a1$2493$rW3Dr)
                                           (begin
                                             '#f
                                             (g$procedure?$16422$r8tjl
                                               g$a2$2494$rW3Dr))
                                           '#f)
                                       ((case-lambda
                                          ((g$port$2537$rW3Dr
                                             g$read-proc$2538$rW3Dr)
                                           ((case-lambda
                                              ((g$var$2539$rW3Dr
                                                 g$ne2$2540$rW3Dr)
                                               (case-lambda
                                                 ((g$empty$2541$rW3Dr)
                                                  (if (if (not (eof-object?
                                                                 g$var$2539$rW3Dr))
                                                        (begin
                                                          '#f
                                                          g$ne2$2540$rW3Dr)
                                                        '#f)
                                                    ((case-lambda
                                                       ((g$value$2542$rW3Dr)
                                                        (begin
                                                          (set! g$var$2539$rW3Dr
                                                            (g$read-proc$2538$rW3Dr
                                                              g$port$2537$rW3Dr))
                                                          g$value$2542$rW3Dr)))
                                                     g$var$2539$rW3Dr)
                                                    g$empty$2541$rW3Dr)))))
                                            (g$read-proc$2538$rW3Dr
                                              g$port$2537$rW3Dr)
                                            '#t)))
                                        g$a1$2493$rW3Dr
                                        g$a2$2494$rW3Dr)
                                       ((case-lambda
                                          (()
                                           (begin '#f '#f)))))))))))))
                      (car g$args$2450$rW3Dr)
                      (g$cadr$16458$r8tjl g$args$2450$rW3Dr))
                     (if (eqv? g$t$2451$rW3Dr '3)
                       ((case-lambda
                          ((g$a1$2543$rW3Dr
                             g$a2$2544$rW3Dr
                             g$a3$2545$rW3Dr)
                           (if (if (g$list?$15989$r8tjl g$a1$2543$rW3Dr)
                                 (if (g$list?$15989$r8tjl
                                       g$a2$2544$rW3Dr)
                                   (begin
                                     '#f
                                     (g$list?$15989$r8tjl
                                       g$a3$2545$rW3Dr))
                                   '#f)
                                 '#f)
                             ((case-lambda
                                ((g$t$2546$rW3Dr g$ne2$2547$rW3Dr)
                                 ((case-lambda
                                    ((g$var$2548$rW3Dr)
                                     (case-lambda
                                       ((g$empty$2549$rW3Dr)
                                        (if (if (not (null?
                                                       g$t$2546$rW3Dr))
                                              (begin
                                                '#f
                                                g$ne2$2547$rW3Dr)
                                              '#f)
                                          (begin
                                            (set! g$var$2548$rW3Dr
                                              (car g$t$2546$rW3Dr))
                                            ((case-lambda
                                               ((g$value$2550$rW3Dr)
                                                (begin
                                                  (set! g$t$2546$rW3Dr
                                                    (cdr g$t$2546$rW3Dr))
                                                  g$value$2550$rW3Dr)))
                                             g$var$2548$rW3Dr))
                                          g$empty$2549$rW3Dr)))))
                                  '#f)))
                              (g$append$16000$r8tjl
                                g$a1$2543$rW3Dr
                                g$a2$2544$rW3Dr
                                g$a3$2545$rW3Dr)
                              '#t)
                             (if (if (g$string?$16416$r8tjl
                                       g$a1$2543$rW3Dr)
                                   (if (g$string?$16416$r8tjl
                                         g$a2$2544$rW3Dr)
                                     (begin
                                       '#f
                                       (g$string?$16416$r8tjl
                                         g$a3$2545$rW3Dr))
                                     '#f)
                                   '#f)
                               ((case-lambda
                                  ((g$str$2551$rW3Dr g$len$2552$rW3Dr)
                                   (begin
                                     (set! g$len$2552$rW3Dr
                                       (g$string-length$16432$r8tjl
                                         g$str$2551$rW3Dr))
                                     ((case-lambda
                                        ((g$i$2553$rW3Dr
                                           g$ne2$2554$rW3Dr)
                                         ((case-lambda
                                            ((g$var$2555$rW3Dr)
                                             (case-lambda
                                               ((g$empty$2556$rW3Dr)
                                                (if (if (g$<$17021$r8tjl
                                                          g$i$2553$rW3Dr
                                                          g$len$2552$rW3Dr)
                                                      (begin
                                                        '#f
                                                        g$ne2$2554$rW3Dr)
                                                      '#f)
                                                  (begin
                                                    (set! g$var$2555$rW3Dr
                                                      (g$string-ref$16429$r8tjl
                                                        g$str$2551$rW3Dr
                                                        g$i$2553$rW3Dr))
                                                    ((case-lambda
                                                       ((g$value$2557$rW3Dr)
                                                        (begin
                                                          (set! g$i$2553$rW3Dr
                                                            (+ g$i$2553$rW3Dr
                                                               '1))
                                                          g$value$2557$rW3Dr)))
                                                     g$var$2555$rW3Dr))
                                                  g$empty$2556$rW3Dr)))))
                                          '#f)))
                                      '0
                                      '#t))))
                                (g$string-append$16438$r8tjl
                                  g$a1$2543$rW3Dr
                                  g$a2$2544$rW3Dr
                                  g$a3$2545$rW3Dr)
                                '0)
                               (if (if (g$vector?$16418$r8tjl
                                         g$a1$2543$rW3Dr)
                                     (if (g$vector?$16418$r8tjl
                                           g$a2$2544$rW3Dr)
                                       (begin
                                         '#f
                                         (g$vector?$16418$r8tjl
                                           g$a3$2545$rW3Dr))
                                       '#f)
                                     '#f)
                                 ((case-lambda
                                    ((g$vec$2558$rW3Dr
                                       g$len$2559$rW3Dr
                                       g$vecs$2560$rW3Dr)
                                     ((case-lambda
                                        ((g$k$2561$rW3Dr
                                           g$ne2$2562$rW3Dr)
                                         ((case-lambda
                                            ((g$var$2563$rW3Dr)
                                             (case-lambda
                                               ((g$empty$2564$rW3Dr)
                                                (if (if (if (g$<$17021$r8tjl
                                                              g$k$2561$rW3Dr
                                                              g$len$2559$rW3Dr)
                                                          '#t
                                                          (if (null?
                                                                g$vecs$2560$rW3Dr)
                                                            '#f
                                                            (begin
                                                              (set! g$vec$2558$rW3Dr
                                                                (car g$vecs$2560$rW3Dr))
                                                              (set! g$vecs$2560$rW3Dr
                                                                (cdr g$vecs$2560$rW3Dr))
                                                              (set! g$len$2559$rW3Dr
                                                                (g$vector-length$16452$r8tjl
                                                                  g$vec$2558$rW3Dr))
                                                              (set! g$k$2561$rW3Dr
                                                                '0)
                                                              '#t)))
                                                      (begin
                                                        '#f
                                                        g$ne2$2562$rW3Dr)
                                                      '#f)
                                                  (begin
                                                    (set! g$var$2563$rW3Dr
                                                      (g$vector-ref$16449$r8tjl
                                                        g$vec$2558$rW3Dr
                                                        g$k$2561$rW3Dr))
                                                    ((case-lambda
                                                       ((g$value$2565$rW3Dr)
                                                        (begin
                                                          (set! g$k$2561$rW3Dr
                                                            (+ g$k$2561$rW3Dr
                                                               '1))
                                                          g$value$2565$rW3Dr)))
                                                     g$var$2563$rW3Dr))
                                                  g$empty$2564$rW3Dr)))))
                                          '#f)))
                                      '0
                                      '#t)))
                                  '#f
                                  '0
                                  (g$ec-:vector-filter$1024$rW3Dr
                                    (list
                                      g$a1$2543$rW3Dr
                                      g$a2$2544$rW3Dr
                                      g$a3$2545$rW3Dr)))
                                 (if (if (g$integer?$17007$r8tjl
                                           g$a1$2543$rW3Dr)
                                       (if (g$exact?$17002$r8tjl
                                             g$a1$2543$rW3Dr)
                                         (if (g$integer?$17007$r8tjl
                                               g$a2$2544$rW3Dr)
                                           (if (g$exact?$17002$r8tjl
                                                 g$a2$2544$rW3Dr)
                                             (if (g$integer?$17007$r8tjl
                                                   g$a3$2545$rW3Dr)
                                               (begin
                                                 '#f
                                                 (g$exact?$17002$r8tjl
                                                   g$a3$2545$rW3Dr))
                                               '#f)
                                             '#f)
                                           '#f)
                                         '#f)
                                       '#f)
                                   ((case-lambda
                                      ((g$a$2566$rW3Dr
                                         g$b$2567$rW3Dr
                                         g$s$2568$rW3Dr
                                         g$stop$2569$rW3Dr)
                                       (begin
                                         (if (not (if (g$integer?$17007$r8tjl
                                                        g$a$2566$rW3Dr)
                                                    (if (g$exact?$17002$r8tjl
                                                          g$a$2566$rW3Dr)
                                                      (if (g$integer?$17007$r8tjl
                                                            g$b$2567$rW3Dr)
                                                        (if (g$exact?$17002$r8tjl
                                                              g$b$2567$rW3Dr)
                                                          (if (g$integer?$17007$r8tjl
                                                                g$s$2568$rW3Dr)
                                                            (begin
                                                              '#f
                                                              (g$exact?$17002$r8tjl
                                                                g$s$2568$rW3Dr))
                                                            '#f)
                                                          '#f)
                                                        '#f)
                                                      '#f)
                                                    '#f))
                                           (g$error$54$rW3Dr
                                             '"arguments of :range are not exact integer "
                                             '"(use :real-range?)"
                                             g$a$2566$rW3Dr
                                             g$b$2567$rW3Dr
                                             g$s$2568$rW3Dr)
                                           (void))
                                         (if (g$zero?$17011$r8tjl
                                               g$s$2568$rW3Dr)
                                           (g$error$54$rW3Dr
                                             '"step size must not be zero in :range")
                                           (void))
                                         (set! g$stop$2569$rW3Dr
                                           (+ g$a$2566$rW3Dr
                                              (* (g$max$16487$r8tjl
                                                   '0
                                                   (g$ceiling$17048$r8tjl
                                                     (/ (- g$b$2567$rW3Dr
                                                           g$a$2566$rW3Dr)
                                                        g$s$2568$rW3Dr)))
                                                 g$s$2568$rW3Dr)))
                                         ((case-lambda
                                            ((g$var$2570$rW3Dr
                                               g$ne2$2571$rW3Dr)
                                             (case-lambda
                                               ((g$empty$2572$rW3Dr)
                                                (if (if (not (g$=$17020$r8tjl
                                                               g$var$2570$rW3Dr
                                                               g$stop$2569$rW3Dr))
                                                      (begin
                                                        '#f
                                                        g$ne2$2571$rW3Dr)
                                                      '#f)
                                                  ((case-lambda
                                                     ((g$value$2573$rW3Dr)
                                                      (begin
                                                        (set! g$var$2570$rW3Dr
                                                          (+ g$var$2570$rW3Dr
                                                             g$s$2568$rW3Dr))
                                                        g$value$2573$rW3Dr)))
                                                   g$var$2570$rW3Dr)
                                                  g$empty$2572$rW3Dr)))))
                                          g$a$2566$rW3Dr
                                          '#t))))
                                    g$a1$2543$rW3Dr
                                    g$a2$2544$rW3Dr
                                    g$a3$2545$rW3Dr
                                    '0)
                                   (if (if (g$real?$17005$r8tjl
                                             g$a1$2543$rW3Dr)
                                         (if (g$real?$17005$r8tjl
                                               g$a2$2544$rW3Dr)
                                           (begin
                                             '#f
                                             (g$real?$17005$r8tjl
                                               g$a3$2545$rW3Dr))
                                           '#f)
                                         '#f)
                                     ((case-lambda
                                        ((g$a$2574$rW3Dr
                                           g$b$2575$rW3Dr
                                           g$s$2576$rW3Dr
                                           g$istop$2577$rW3Dr)
                                         (begin
                                           (if (not (if (g$real?$17005$r8tjl
                                                          g$a$2574$rW3Dr)
                                                      (if (g$real?$17005$r8tjl
                                                            g$b$2575$rW3Dr)
                                                        (begin
                                                          '#f
                                                          (g$real?$17005$r8tjl
                                                            g$s$2576$rW3Dr))
                                                        '#f)
                                                      '#f))
                                             (g$error$54$rW3Dr
                                               '"arguments of :real-range are not real"
                                               g$a$2574$rW3Dr
                                               g$b$2575$rW3Dr
                                               g$s$2576$rW3Dr)
                                             (void))
                                           (if (if (g$exact?$17002$r8tjl
                                                     g$a$2574$rW3Dr)
                                                 (begin
                                                   '#f
                                                   ((case-lambda
                                                      ((g$t$2578$rW3Dr)
                                                       (if g$t$2578$rW3Dr
                                                         g$t$2578$rW3Dr
                                                         (begin
                                                           '#f
                                                           (not (g$exact?$17002$r8tjl
                                                                  g$s$2576$rW3Dr))))))
                                                    (not (g$exact?$17002$r8tjl
                                                           g$b$2575$rW3Dr))))
                                                 '#f)
                                             (set! g$a$2574$rW3Dr
                                               (g$exact->inexact$17746$r8tjl
                                                 g$a$2574$rW3Dr))
                                             (void))
                                           (set! g$istop$2577$rW3Dr
                                             (/ (- g$b$2575$rW3Dr
                                                   g$a$2574$rW3Dr)
                                                g$s$2576$rW3Dr))
                                           ((case-lambda
                                              ((g$i$2579$rW3Dr
                                                 g$ne2$2580$rW3Dr)
                                               ((case-lambda
                                                  ((g$var$2581$rW3Dr)
                                                   (case-lambda
                                                     ((g$empty$2582$rW3Dr)
                                                      (if (if (g$<$17021$r8tjl
                                                                g$i$2579$rW3Dr
                                                                g$istop$2577$rW3Dr)
                                                            (begin
                                                              '#f
                                                              g$ne2$2580$rW3Dr)
                                                            '#f)
                                                        (begin
                                                          (set! g$var$2581$rW3Dr
                                                            (+ g$a$2574$rW3Dr
                                                               (* g$s$2576$rW3Dr
                                                                  g$i$2579$rW3Dr)))
                                                          ((case-lambda
                                                             ((g$value$2583$rW3Dr)
                                                              (begin
                                                                (set! g$i$2579$rW3Dr
                                                                  (+ g$i$2579$rW3Dr
                                                                     '1))
                                                                g$value$2583$rW3Dr)))
                                                           g$var$2581$rW3Dr))
                                                        g$empty$2582$rW3Dr)))))
                                                '#f)))
                                            '0
                                            '#t))))
                                      g$a1$2543$rW3Dr
                                      g$a2$2544$rW3Dr
                                      g$a3$2545$rW3Dr
                                      '0)
                                     ((case-lambda
                                        (() (begin '#f '#f)))))))))))
                        (car g$args$2450$rW3Dr)
                        (g$cadr$16458$r8tjl g$args$2450$rW3Dr)
                        (g$caddr$16464$r8tjl g$args$2450$rW3Dr))
                       ((case-lambda
                          (()
                           (begin
                             '#f
                             (letrec ((g$every?$2584$rW3Dr
                                        (case-lambda
                                          ((g$pred$2602$rW3Dr
                                             g$args$2603$rW3Dr)
                                           (if (null? g$args$2603$rW3Dr)
                                             '#t
                                             (if (g$pred$2602$rW3Dr
                                                   (car g$args$2603$rW3Dr))
                                               (begin
                                                 '#f
                                                 (g$every?$2584$rW3Dr
                                                   g$pred$2602$rW3Dr
                                                   (cdr g$args$2603$rW3Dr)))
                                               '#f))))))
                               (if (g$every?$2584$rW3Dr
                                     g$list?$15989$r8tjl
                                     g$args$2450$rW3Dr)
                                 ((case-lambda
                                    ((g$t$2585$rW3Dr g$ne2$2586$rW3Dr)
                                     ((case-lambda
                                        ((g$var$2587$rW3Dr)
                                         (case-lambda
                                           ((g$empty$2588$rW3Dr)
                                            (if (if (not (null?
                                                           g$t$2585$rW3Dr))
                                                  (begin
                                                    '#f
                                                    g$ne2$2586$rW3Dr)
                                                  '#f)
                                              (begin
                                                (set! g$var$2587$rW3Dr
                                                  (car g$t$2585$rW3Dr))
                                                ((case-lambda
                                                   ((g$value$2589$rW3Dr)
                                                    (begin
                                                      (set! g$t$2585$rW3Dr
                                                        (cdr g$t$2585$rW3Dr))
                                                      g$value$2589$rW3Dr)))
                                                 g$var$2587$rW3Dr))
                                              g$empty$2588$rW3Dr)))))
                                      '#f)))
                                  (apply
                                    g$append$16000$r8tjl
                                    g$args$2450$rW3Dr)
                                  '#t)
                                 (if (g$every?$2584$rW3Dr
                                       g$string?$16416$r8tjl
                                       g$args$2450$rW3Dr)
                                   ((case-lambda
                                      ((g$str$2590$rW3Dr
                                         g$len$2591$rW3Dr)
                                       (begin
                                         (set! g$len$2591$rW3Dr
                                           (g$string-length$16432$r8tjl
                                             g$str$2590$rW3Dr))
                                         ((case-lambda
                                            ((g$i$2592$rW3Dr
                                               g$ne2$2593$rW3Dr)
                                             ((case-lambda
                                                ((g$var$2594$rW3Dr)
                                                 (case-lambda
                                                   ((g$empty$2595$rW3Dr)
                                                    (if (if (g$<$17021$r8tjl
                                                              g$i$2592$rW3Dr
                                                              g$len$2591$rW3Dr)
                                                          (begin
                                                            '#f
                                                            g$ne2$2593$rW3Dr)
                                                          '#f)
                                                      (begin
                                                        (set! g$var$2594$rW3Dr
                                                          (g$string-ref$16429$r8tjl
                                                            g$str$2590$rW3Dr
                                                            g$i$2592$rW3Dr))
                                                        ((case-lambda
                                                           ((g$value$2596$rW3Dr)
                                                            (begin
                                                              (set! g$i$2592$rW3Dr
                                                                (+ g$i$2592$rW3Dr
                                                                   '1))
                                                              g$value$2596$rW3Dr)))
                                                         g$var$2594$rW3Dr))
                                                      g$empty$2595$rW3Dr)))))
                                              '#f)))
                                          '0
                                          '#t))))
                                    (apply
                                      g$string-append$16438$r8tjl
                                      g$args$2450$rW3Dr)
                                    '0)
                                   (if (g$every?$2584$rW3Dr
                                         g$vector?$16418$r8tjl
                                         g$args$2450$rW3Dr)
                                     ((case-lambda
                                        ((g$t$2597$rW3Dr
                                           g$ne2$2598$rW3Dr)
                                         ((case-lambda
                                            ((g$var$2599$rW3Dr)
                                             (case-lambda
                                               ((g$empty$2600$rW3Dr)
                                                (if (if (not (null?
                                                               g$t$2597$rW3Dr))
                                                      (begin
                                                        '#f
                                                        g$ne2$2598$rW3Dr)
                                                      '#f)
                                                  (begin
                                                    (set! g$var$2599$rW3Dr
                                                      (car g$t$2597$rW3Dr))
                                                    ((case-lambda
                                                       ((g$value$2601$rW3Dr)
                                                        (begin
                                                          (set! g$t$2597$rW3Dr
                                                            (cdr g$t$2597$rW3Dr))
                                                          g$value$2601$rW3Dr)))
                                                     g$var$2599$rW3Dr))
                                                  g$empty$2600$rW3Dr)))))
                                          '#f)))
                                      (apply
                                        g$append$16000$r8tjl
                                        (g$map$15994$r8tjl
                                          g$vector->list$16454$r8tjl
                                          g$args$2450$rW3Dr))
                                      '#t)
                                     ((case-lambda
                                        (()
                                         (begin
                                           '#f
                                           '#f))))))))))))))))))
            (g$length$15991$r8tjl g$args$2450$rW3Dr)))))))
   (g$:-dispatch$1631$rW3Dr
     g$:-dispatch$2610$rW3Dr
     (g$make-parameter$20331$r8tjl
       (g$make-initial-:-dispatch$1630$rW3Dr)
       (case-lambda
         ((g$x$2604$rW3Dr)
          (if (g$procedure?$16422$r8tjl g$x$2604$rW3Dr)
            g$x$2604$rW3Dr
            (g$error$54$rW3Dr '"not a procedure" g$x$2604$rW3Dr))))))
   (g$:-dispatch-ref$1632$rW3Dr
     g$:-dispatch-ref$2611$rW3Dr
     (case-lambda (() (g$:-dispatch$1631$rW3Dr))))
   (g$:-dispatch-set!$1633$rW3Dr
     g$:-dispatch-set!$2612$rW3Dr
     (case-lambda
       ((g$dispatch$2605$rW3Dr)
        (g$:-dispatch$1631$rW3Dr g$dispatch$2605$rW3Dr)))))
  (void))


