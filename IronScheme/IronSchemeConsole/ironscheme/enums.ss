(library (ironscheme enums)
  (export make-enumeration
          enum-set-universe
          enum-set-indexer
          enum-set-constructor
          enum-set->list
          enum-set-member?
          enum-set-subset?
          enum-set=?
          enum-set-union
          enum-set-intersection
          enum-set-difference
          enum-set-complement
          enum-set-projection)
  (import 
    (except (rnrs)
      make-enumeration 
      enum-set-universe 
      enum-set=? 
      enum-set->list 
      enum-set-complement 
      enum-set-constructor
      enum-set-difference
      enum-set-indexer
      enum-set-intersection
      enum-set-member?
      enum-set-projection
      enum-set-subset?
      enum-set-union)
      (only (ironscheme) fprintf)
      (only (ironscheme core) reverse!) ; for reverse!
      (ironscheme records printer)) 
 
   ; type for enumerations
  (define-record-type enum (fields value info))

  (define-record-type enum-universe (fields symbols mapping value))

  (define (enum-type=? enumset1 enumset2)
    (eq? (enum-info enumset1) (enum-info enumset2)))

  (define (get-symbols enumset)
    (enum-universe-symbols (enum-info enumset)))

  (define (get-value enumset s)
    (hashtable-ref
      (enum-universe-mapping (enum-info enumset)) s #f))

  (define (assert-enum who enumset)
    (unless (enum? enumset)
      (assertion-violation who "not an enumeration" enumset)))

  (define (distinct symbols)
    (let ((ht (make-eq-hashtable)))
      (let f ((s symbols)(a '()))
        (if (null? s)
          (reverse! a)
          (if (hashtable-contains? ht (car s))
            (f (cdr s) a)
            (begin
              (hashtable-set! ht (car s) #t)
              (f (cdr s) (cons (car s) a))))))))

  (define (make-enumeration symbols)
    (unless (for-all symbol? symbols)
      (assertion-violation 'make-enumeration "not a list of symbols" symbols))
    (let ((s (distinct symbols))
          (mask 1)
          (intmap (make-eq-hashtable)))
      (for-each
        (lambda (e)
          (hashtable-set! intmap e mask)
          (set! mask (bitwise-arithmetic-shift-left mask 1)))
        s)
      (make-enum
        (- mask 1)
        (make-enum-universe s intmap (- mask 1)))))

  (define (enum-set-universe enumset)
    (assert-enum 'enum-set-universe enumset)
    (let ((ei (enum-info enumset)))
      (make-enum (enum-universe-value ei) ei)))

  (define (enum-set-indexer enumset)
    (assert-enum 'enum-set-indexer enumset)
    (lambda (symbol)
      (unless (symbol? symbol)
        (assertion-violation 'enum-set-indexer "not a symbol" symbol))
      (let ((v (get-value enumset symbol)))
        (if v
          (- (bitwise-length v) 1)
          #f))))

  (define (enum-set-constructor enumset)
    (assert-enum 'enum-set-constructor enumset)
    (lambda (symbols)
      (unless (for-all symbol? symbols)
        (assertion-violation 'enum-set-constructor "not a list of symbols" symbols))
      (let f ((v 0)(s symbols))
        (if (null? s)
          (make-enum v (enum-info enumset))
          (let ((v* (get-value enumset (car s))))
            (if v*
              (f (bitwise-ior v v*) (cdr s))
              (assertion-violation
                'enum-set-constructor
                "not a member of enum-set"
                (car s))))))))

  (define (enum-set->list enumset)
    (assert-enum 'enum-set->list enumset)
    (let ((value (enum-value enumset)))
      (let f ((s (get-symbols enumset))(l '()))
        (if (null? s)
          (reverse! l)
          (if (zero? (bitwise-and (get-value enumset (car s)) value))
            (f (cdr s) l)
            (f (cdr s) (cons (car s) l)))))))

  (define (enum-set-member? symbol enumset)
    (unless (symbol? symbol)
      (assertion-violation 'enum-set-member? "not a symbol" symbol))
    (assert-enum 'enum-set-member? enumset)
    (let ((v (get-value enumset symbol)))
      (if v
        (not (zero? (bitwise-and v (enum-value enumset))))
        #f)))

  (define (enum-set-subset? enumset1 enumset2)
    (assert-enum 'enum-set-subset? enumset1)
    (assert-enum 'enum-set-subset? enumset2)
    (let ((v1 (enum-value enumset1))
          (v2 (enum-value enumset2)))
         (if (enum-type=? enumset1 enumset2)
            (= (bitwise-and v1 v2) v1)
            (let f ((s (get-symbols enumset1)))
              (if (null? s)
                #t
                (let ((v1* (get-value enumset1 (car s)))
                      (v2* (get-value enumset2 (car s))))
                  (if v2*
                    (let ((has1 (not (zero? (bitwise-and v1 v1*))))
                          (has2 (not (zero? (bitwise-and v2 v2*)))))
                       (if (and has1 (not has2))
                          #f
                          (f (cdr s))))
                    #f)))))))

  (define (enum-set=? enumset1 enumset2)
    (assert-enum 'enum-set=? enumset1)
    (assert-enum 'enum-set=? enumset2)
    (and
      (enum-set-subset? enumset1 enumset2)
      (enum-set-subset? enumset2 enumset1)))

  (define (enum-set-union enumset1 enumset2)
    (assert-enum 'enum-set-union enumset1)
    (assert-enum 'enum-set-union enumset2)
    (if (enum-type=? enumset1 enumset2)
      (let ((v1 (enum-value enumset1))
            (v2 (enum-value enumset2)))
        (make-enum
          (bitwise-ior v1 v2)
          (enum-info enumset1)))
      #f))

  (define (enum-set-intersection enumset1 enumset2)
    (assert-enum 'enum-set-intersection enumset1)
    (assert-enum 'enum-set-intersection enumset2)
    (if (enum-type=? enumset1 enumset2)
      (let ((v1 (enum-value enumset1))
            (v2 (enum-value enumset2)))
        (make-enum
          (bitwise-and v1 v2)
          (enum-info enumset1)))
      #f))

  (define (enum-set-difference enumset1 enumset2)
    (assert-enum 'enum-set-difference enumset1)
    (assert-enum 'enum-set-difference enumset2)
    (if (enum-type=? enumset1 enumset2)
      (let ((v1 (enum-value enumset1))
            (v2 (enum-value enumset2)))
        (make-enum
          (bitwise-xor v1 (bitwise-and v1 v2))
          (enum-info enumset1)))
      #f))

  (define (enum-set-complement enumset)
    (assert-enum 'enum-set-complement enumset)
    (let ((v (enum-value enumset))
          (u (enum-value (enum-set-universe enumset))))
      (make-enum
        (bitwise-and (bitwise-not v) u)
        (enum-info enumset))))

  (define (enum-set-projection enumset1 enumset2)
    (assert-enum 'enum-set-projection enumset1)
    (assert-enum 'enum-set-projection enumset2)
    (let ((v1 (enum-value enumset1)))
       (let f ((s (get-symbols enumset1))(v 0))
          (if (null? s)
            (make-enum v (enum-info enumset2))
            (if (zero? (bitwise-and v1 (get-value enumset1 (car s))))
              (f (cdr s) v)
              (let ((v2 (get-value enumset2 (car s))))
                (if v2
                  (f (cdr s) (bitwise-ior v v2))
                  (f (cdr s) v))))))))

   (make-record-printer 'enum 
     (lambda (x p)
       (fprintf p "#[enum-set ~a]" (enum-set->list x))))  
)      
    
