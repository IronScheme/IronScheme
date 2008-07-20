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
    (except (ironscheme)
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
      (ironscheme records printer)) 
 
  ; rtd -> hashtable(symbol -> int)
  (define enummap (make-eq-hashtable))    
  ; rtd -> symbol list
  (define enumordermap (make-eq-hashtable))
  ; rtd -> rcd
  (define rcdmap (make-eq-hashtable))
  ; base type for enumerations
  (define-record-type enum (fields value)) 

  (define-syntax make-enum-type
    (lambda (x)
      (syntax-case x ()
        [(k) 
          (with-syntax ((name (datum->syntax #'k (gensym))))
            #'(lambda ()
                (define-record-type name (parent enum) (sealed #t))
                (make-record-printer 'name 
                  (lambda (x p)
                    (fprintf p "#[enum-set ~a]" (enum-set->list x))))
                (values
                  (record-type-descriptor name)
                  (record-constructor-descriptor name))))])))
                  
  (define (get-symbols rtd)
    (hashtable-ref enumordermap rtd #f))  
    
  (define (get-value rtd s)
    (hashtable-ref 
      (hashtable-ref enummap rtd #f) s #f))    
      
  (define (construct rtd value)
    ((record-constructor (hashtable-ref rcdmap rtd #f)) value))

  (define (make-enumeration symbols)
    (call-with-values (make-enum-type)
      (lambda (rtd rcd)
        (let ((s symbols)
              (mask 1)
              (intmap (make-eq-hashtable)))
          (hashtable-set! rcdmap rtd rcd)
          (hashtable-set! enumordermap rtd s)
          (for-each   
            (lambda (e)
              (hashtable-set! intmap e mask)
              (set! mask (bitwise-arithmetic-shift-left mask 1)))
            s)
          (hashtable-set! enummap rtd intmap)
          ((record-constructor rcd) 
            (- (bitwise-arithmetic-shift-left 1 (length s)) 1))))))

  (define (enum-set-universe enumset)
    (define rtd (record-rtd enumset))
    (define count (length (get-symbols rtd)))
    (construct rtd 
      (- (bitwise-arithmetic-shift-left 1 count) 1)))

  (define (enum-set-indexer enumset)
    (lambda (symbol)
      (let f ((s (get-symbols (record-rtd enumset)))
              (i 0))
        (if (null? s) 
          #f
          (if (eq? (car s) symbol) 
            i
            (f (cdr s) (+ i 1)))))))
          
  (define (enum-set-constructor enumset)
    (let* ((rtd (record-rtd enumset)))
      (lambda (symbols)
        (let f ((v 0)(s symbols))
          (if (null? s)
            (construct rtd v)
            (f (bitwise-ior v 
                (get-value rtd (car s)))
               (cdr s)))))))

  (define (enum-set->list enumset)
    (let ((rtd (record-rtd enumset))
          (value (enum-value enumset)))
      (let f ((s (get-symbols rtd))(l '()))
        (if (null? s)
          (reverse l)
          (if (zero? (bitwise-and (get-value rtd (car s)) value))
            (f (cdr s) l)
            (f (cdr s) (cons (car s) l)))))))

  (define (enum-set-member? symbol enumset)
    (not (zero? (bitwise-and 
                  (get-value (record-rtd enumset) symbol) 
                  (enum-value enumset)))))
                  
  (define (enum-set-subset? enumset1 enumset2)
    (let ((v1 (enum-value enumset1))
          (v2 (enum-value enumset2))
          (rtd1 (record-rtd enumset1))
          (rtd2 (record-rtd enumset2)))
         (if (eq? rtd1 rtd2)
            (= (bitwise-and v1 v2) v1)
            (let f ((s (get-symbols rtd1)))
              (if (null? s)
                #t
                (let ((v1* (get-value rtd1 (car s)))
                      (v2* (get-value rtd2 (car s))))
                  (if v2*
                    (let ((has1 (not (zero? (bitwise-and v1 v1*))))
                          (has2 (not (zero? (bitwise-and v2 v2*)))))
                       (if (and has1 (not has2))
                          #f
                          (f (cdr s))))
                    #f)))))))
                    
  (define (enum-set=? enumset1 enumset2)
    (and 
      (enum-set-subset? enumset1 enumset2) 
      (enum-set-subset? enumset2 enumset1)))
      
  (define (enum-set-union enumset1 enumset2)
    (let ((v1 (enum-value enumset1))
          (v2 (enum-value enumset2))
          (rtd1 (record-rtd enumset1))
          (rtd2 (record-rtd enumset2)))
      (if (eq? rtd1 rtd2)
        (construct rtd1
          (bitwise-ior v1 v2))
        #f)))

  (define (enum-set-intersection enumset1 enumset2)
    (let ((v1 (enum-value enumset1))
          (v2 (enum-value enumset2))
          (rtd1 (record-rtd enumset1))
          (rtd2 (record-rtd enumset2)))
      (if (eq? rtd1 rtd2)
        (construct rtd1
          (bitwise-and v1 v2))
        #f)))
        
  (define (enum-set-difference enumset1 enumset2)
    (let ((v1 (enum-value enumset1))
          (v2 (enum-value enumset2))
          (rtd1 (record-rtd enumset1))
          (rtd2 (record-rtd enumset2)))
      (if (eq? rtd1 rtd2)
        (construct rtd1
          (bitwise-xor v1 v2))
        #f)))   

  (define (enum-set-complement enumset)
    (enum-set-difference enumset (enum-set-universe enumset)))
        
  (define (enum-set-projection enumset1 enumset2)
    (let ((v1 (enum-value enumset1))
          (rtd1 (record-rtd enumset1))
          (rtd2 (record-rtd enumset2)))
       (let f ((s (get-symbols rtd1))(v 0))
          (if (null? s)
            (construct rtd2 v)
            (if (zero? (bitwise-and v1 (get-value rtd1 (car s))))
              (f (cdr s) v)
              (let ((v2 (get-value rtd2 (car s))))
                (if (v2)
                  (f (cdr s) (bitwise-ior v v2))
                  (f (cdr s) v))))))))
      
)      
    