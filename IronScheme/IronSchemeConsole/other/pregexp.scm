;pregexp.scm
;Portable regular expressions for Scheme
;Dorai Sitaram
;http://www.cs.rice.edu/~dorai
;ds26@gte.com
;Oct 2, 1999

(define pregexp-compile
  (lambda (s)
    (let ((n (string-length s)))
      (let loop ((r '()) (i 0))
        (if (>= i n) (cons ':seq (reverse r))
            (let ((vv (pregexp-read-chunk s i n)))
              (loop (cons (car vv) r) (cdr vv))))))))

;

(define pregexp-read-num
  (lambda (s i)
    (let loop ((r '()) (k i))
      (let ((c (string-ref s k)))
        (if (char-numeric? c)
            (loop (cons c r) (+ k 1))
            (cons (string->number (list->string (reverse r)))
                  k))))))

(define pregexp-wrap-multiplier-if-any
  (lambda (s vv n)
    (let ((i (cdr vv)))
      (case (and (< i n) (string-ref s i))
        ((#\*) (cons (list ':zero-or-more (car vv)) (+ i 1)))
        ((#\+) (cons (list ':one-or-more (car vv)) (+ i 1)))
        ((#\?) (cons (list ':zero-or-one (car vv)) (+ i 1)))
        ((#\\) (case (string-ref s (+ i 1))
                 ((#\{)
                  (let* ((m (pregexp-read-num s (+ i 2)))
                         (n (pregexp-read-num s (+ (cdr m) 1))))
                    (cons (list ':between-nums (car vv) (car m) (car n))
                          (+ (cdr n) 2))))
                 (else vv)))
        (else vv)))))

(define pregexp-invert-char-list
  (lambda (vv)
    (set-car! (car vv) ':none-of-chars)
    vv))

;

(define pregexp-read-char-list
  (lambda (s i n)
    (let loop ((r '()) (i i))
      (let ((c (string-ref s i)))
        (case c
          ((#\]) (cons (cons ':one-of-chars (reverse r)) (+ i 1)))
          ((#\\) (loop (cons (string-ref s (+ i 1)) r) (+ i 2)))
          ((#\-) (loop (cons (list ':char-range (car r)
                                   (string-ref s (+ i 1)))
                             (cdr r))
                       (+ i 2)))
          (else (loop (cons c r) (+ i 1))))))))

(define pregexp-read-chunk
  (lambda (s i n)
    (let ((c (string-ref s i)))
      (case c
        ((#\^) (cons ':bos (+ i 1)))
        ((#\$) (cons ':eos (+ i 1)))
        ((#\.) (pregexp-wrap-multiplier-if-any
                s
                (cons ':any (+ i 1)) n))
        ((#\[) (pregexp-wrap-multiplier-if-any
                s
                (case (string-ref s (+ i 1))
                  ((#\^) (pregexp-invert-char-list
                          (pregexp-read-char-list s (+ i 2) n)))
                  (else (pregexp-read-char-list s (+ i 1) n)))
                n))
        ((#\\) (pregexp-wrap-multiplier-if-any
                s
                (let ((c (string-ref s (+ i 1))))
                  (case c
                    ((#\() (pregexp-read-sub-pat s (+ i 2) n))
                    (else (cons c (+ i 2)))))
                n))
        (else (pregexp-wrap-multiplier-if-any
               s (cons c (+ i 1)) n))))))

(define pregexp-read-sub-pat
  (lambda (s i n)
    (let loop ((r '()) (i i))
      (case (string-ref s i)
        ((#\\) (case (string-ref s (+ i 1))
                 ((#\)) (cons (list ':sub (cons ':seq (reverse r)))
                              (+ i 2)))
                 (else (let ((vv (pregexp-read-chunk s i n)))
                         (loop (cons (car vv) r) (cdr vv))))))
        (else (let ((vv (pregexp-read-chunk s i n)))
                (loop (cons (car vv) r) (cdr vv))))))))

;

(define pregexp-match
  (lambda (pat str)
    (pregexp-match-precompiled
     (if (string? pat) (pregexp-compile pat) pat)
     str)))

(define pregexp-match-precompiled
  (lambda (re s)
    (let ((n (string-length s)))
      (let loop ((i 0))
        (and (< i n)
             (or (pregexp-match-anchoring-left re s i n)
                 (loop (+ i 1))))))))

(define pregexp-match-anchoring-left
  (lambda (re s i n)
    (let* ((sub-match-stk '())
           (collect-sub-matches? #t)
           (dcsm
            ;= don't collect sub-matches, but that's too long a name.
            ;would've preferred fluid-let, but RnRS doesn't have it
            (lambda (th)
              (cond (collect-sub-matches?
                     (set! collect-sub-matches? #f)
                     (let ((res (th)))
                       (set! collect-sub-matches? #t)
                       res))
                    (else (th)))))
           (k
            (let recur ((re re) (i i))
              (cond
               ((not i) #f)
               ((char? re)
                (and (< i n)
                     (char=? (string-ref s i) re)
                     (+ i 1)))
               ((eqv? re ':bos) (and (= i 0) i))
               ((eqv? re ':eos) (and (>= i n) i))
               ((eqv? re ':any) (and (< i n) (+ i 1)))
               (else
                (case (car re)
                  ((:seq)
                   (let seq-recur ((res (cdr re)) (i i))
                     (cond
                      ((not i) #f)
                      ((null? res) i)
                      (else
                       (let ((re (car res)) (rest (cdr res)))
                         (seq-recur 
                          rest
                          (if (and (pair? re) (memv (car re)
                                                    '(:zero-or-one
                                                      :zero-or-more
                                                      :one-or-more
                                                      :between-nums)))
                              (let ((left-match-stk sub-match-stk))
                                ;fluid-let
                                (set! sub-match-stk '())
                                (let ((dir (car re)) (subre (cadr re)))
                                  (let ((i 
                                         (case (car re)
                                           ((:zero-or-one)
                                            (let ((j (recur subre i)))
                                              (or (and j (dcsm
                                                          (lambda ()
                                                            (seq-recur 
                                                             rest j)))
                                                       j)
                                                  (and (dcsm
                                                        (lambda ()
                                                          (seq-recur rest i)))
                                                       i))))
                                           ((:zero-or-more)
                                            (let mul-recur ((i i))
                                              (let ((j (recur subre i)))
                                                (or (and j (mul-recur j))
                                                    (and (dcsm
                                                          (lambda ()
                                                            (seq-recur 
                                                             rest i)))
                                                         i)))))
                                           ((:one-or-more)
                                            (let mul-recur ((i (recur 
                                                                subre i)))
                                              (and i
                                                   (let ((j (recur subre i)))
                                                     (or (and j (mul-recur j))
                                                         (and (dcsm
                                                               (lambda ()
                                                                 (seq-recur 
                                                                  rest i)))
                                                              i))))))
                                           ((:between-nums)
                                            (let ((m (or (caddr re) 0))
                                                  (n (cadddr re)))
                                              ;at least m
                                              (let ((i 
                                                     (let loopm ((i i) (k 0))
                                                       (if (= k m) i
                                                           (let ((j (recur
                                                                     subre i)))
                                                             (and 
                                                              j
                                                              (loopm 
                                                               j
                                                               (+ k 1))))))))
                                                ;at most n
                                                (let ((n-m (and n (- n m))))
                                                  (let mul-recur ((i i) (k 0))
                                                    (and (or (not n)
                                                             (<= k n-m))
                                                         (let ((j (recur 
                                                                   subre i)))
                                                           (or (and 
                                                                j (mul-recur
                                                                   j (+ k 1)))
                                                               (and 
                                                                (dcsm
                                                                 (lambda ()
                                                                   (seq-recur
                                                                    rest i)))
                                                                i))))))))))))
                                    (set! sub-match-stk
                                      (case (length sub-match-stk)
                                        ((0) left-match-stk)
                                        ((1) (cons (car sub-match-stk)
                                                   left-match-stk))
                                        (else (cons (cadr sub-match-stk)
                                                    left-match-stk))))
                                    i)))
                              (recur re i))))))))
                  ((:char-range)
                   (and (< i n)
                        ;(char<=? (cadr re) (string_ref s i) (caddr re))
                        ;would've been nice, but RnRS disallows it
                        (let ((s_i (string-ref s i)))
                          (and (char<=? (cadr re) s_i)
                               (char<=? s_i (caddr re))))
                        (+ i 1)))
                  ((:one-of-chars)
                   (and (< i n)
                        (let loopj ((chars (cdr re)))
                          (cond ((null? chars) #f)
                                ((recur (car chars) i))
                                (else (loopj (cdr chars)))))))
                  ((:none-of-chars)
                   (and (< i n)
                        (let loopj ((chars (cdr re)))
                          (cond ((null? chars) (+ i 1))
                                ((recur (car chars) i) #f)
                                (else (loopj (cdr chars)))))))
                  ((:sub)
                   (let ((j (recur (cadr re) i)))
                     (if collect-sub-matches?
                         (set! sub-match-stk
                           (cons (and j (cons i j))
                                 sub-match-stk)))
                     j))))))))
      (and k (cons (cons i k) (reverse sub-match-stk))))))
