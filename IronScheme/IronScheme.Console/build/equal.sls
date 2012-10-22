

;;; Copied from
;;; Efficient Nondestructive Equality Checking for Trees and Graphs 
;;; Michael D. Adams and R. Kent Dybvig 
;;; ICFP 2008

;;; Then copied from Ikarus.  :)

(library (ironscheme equal)
  (export equal?)
  (import 
    (except (ironscheme) equal?)
    (ironscheme clr))
    
  (define k0 400)
  (define kb -40)    

  (module UNSAFE 
    (< <= > >= = + - vector-ref car cdr)
    (import 
      (rename (ironscheme unsafe)
        ($vector-ref vector-ref)
        ($car   car)
        ($cdr   cdr)
        ($fx+   +)
        ($fx-   -)
        ($fx<?  <)
        ($fx>?  >)
        ($fx>=? >=)
        ($fx<=? <=)
        ($fx=?  =))))

  (define-record-type box (fields (mutable content box-content set-box-content!)))
  
  (define random
    (let ((rg (clr-new Random)))
      (lambda (k)
        (clr-call Random Next rg k))))    

  (define (union-find ht x y)
    (import UNSAFE)
    (define (find b)
      (let ([n (box-content b)])
        (if (box? n)
            (let loop ([b b] [n n])
              (let ([nn (box-content n)])
                (if (box? nn) (begin (set-box-content! b nn) (loop n nn)) n)))
            b)))
    (let ([bx (hashtable-ref ht x #f)]
          [by (hashtable-ref ht y #f)])
      (if (not bx)
          (if (not by)
              (let ([b (make-box 1)])
                (hashtable-set! ht x b)
                (hashtable-set! ht y b)
                #f)
              (let ([ry (find by)]) (hashtable-set! ht x ry) #f))
          (if (not by)
              (let ([rx (find bx)]) (hashtable-set! ht y rx) #f)
              (let ([rx (find bx)] [ry (find by)])
                (or (eq? rx ry)
                    (let ([nx (box-content rx)] [ny (box-content ry)])
                      (if (> nx ny)
                          (begin 
                            (set-box-content! ry rx) 
                            (set-box-content! rx (+ nx ny))
                            #f)
                          (begin
                            (set-box-content! rx ry)
                            (set-box-content! ry (+ ny nx))
                            #f)))))))))

  (define (pre? x y k)
    (import UNSAFE)
    (cond
      [(eq? x y) k]
      [(pair? x)
       (and (pair? y)
            (if (<= k 0)
                k
                (let ([k (pre? (car x) (car y) (- k 1))])
                  (and k (pre? (cdr x) (cdr y) k)))))]
      [(vector? x)
       (and (vector? y)
            (let ([n (vector-length x)])
              (and (= (vector-length y) n)
                   (let f ([i 0] [k k])
                     (if (or (= i n) (<= k 0))
                         k
                         (let ([k (pre?
                                    (vector-ref x i)
                                    (vector-ref y i)
                                    (- k 1))])
                           (and k (f (+ i 1) k))))))))]
      [(string? x) (and (string? y) (string=? x y) k)]
      [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
      [else (and (eqv? x y) k)]))

  (define (interleave? x y k)
    (import UNSAFE)
    (let ([ht #f])
      (define (call-union-find x y)
        (unless ht (set! ht (make-eq-hashtable)))
        (union-find ht x y))
      (define (e? x y k)
        (if (<= k 0)
            (if (= k kb) (fast? x y (random (* 2 k0))) (slow? x y k))
            (fast? x y k)))
      (define (slow? x y k)
        (cond
          [(eq? x y) k]
          [(pair? x)
           (and (pair? y)
                (if (call-union-find x y)
                    0
                    (let ([k (e? (car x) (car y) (- k 1))])
                      (and k (e? (cdr x) (cdr y) k)))))]
          [(vector? x)
           (and (vector? y)
                (let ([n (vector-length x)])
                  (and (= (vector-length y) n)
                       (if (call-union-find x y)
                           0
                           (let f ([i 0] [k (- k 1)])
                             (if (= i n)
                                 k
                                 (let ([k (e? (vector-ref x i)
                                              (vector-ref y i)
                                              k)])
                                   (and k (f (+ i 1) k)))))))))]
          [(string? x) (and (string? y) (string=? x y) k)]
          [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
          [else (and (eqv? x y) k)]))
      (define (fast? x y k)
        (let ([k (- k 1)])
          (cond
            [(eq? x y) k]
            [(pair? x)
             (and (pair? y)
                  (let ([k (e? (car x) (car y) k)])
                    (and k (e? (cdr x) (cdr y) k))))]
            [(vector? x)
             (and (vector? y)
                  (let ([n (vector-length x)])
                    (and (= (vector-length y) n)
                         (let f ([i 0] [k k])
                           (if (= i n)
                               k
                               (let ([k (e? (vector-ref x i)
                                            (vector-ref y i)
                                            k)])
                                 (and k (f (+ i 1) k))))))))]
            [(string? x) (and (string? y) (string=? x y) k)]
            [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
            [else (and (eqv? x y) k)])))
      (and (e? x y k) #t)))


  
  (define (interleave-equal? x y)
    (interleave? x y k0))
  
  (define (precheck/interleave-equal? x y)
    (let ([k (pre? x y k0)])
      (and k (or (> k 0) (interleave? x y 0)))))

  (define (equal? x y)
    (precheck/interleave-equal? x y)))
