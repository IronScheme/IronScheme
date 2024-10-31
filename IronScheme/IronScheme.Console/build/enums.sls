#| License
Copyright (c) 2007-2016 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme enums)
  (export make-enumeration
          enum-set-universe
          enum-set-indexer
          enum-set-constructor
          enum-set->list
          enum-set-member?
          enum-set-subset?
          enum-set=?
          (rename (enum? enum-set?))
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
      (only (ironscheme) fprintf format)
      (only (ironscheme core) reverse!) ; for reverse!
      (ironscheme contracts)
      (ironscheme typed)
      (ironscheme records printer)) 
 
   ; type for enumerations
  (define-record-type enum (fields value info))

  (define-record-type enum-universe (fields id symbols mapping value))

  (define: (enum-type=? enumset1 enumset2 -> bool)
    (eq? 
      (enum-universe-id (enum-info enumset1))
      (enum-universe-id (enum-info enumset2))))

  (define (get-symbols enumset)
    (enum-universe-symbols (enum-info enumset)))

  (define (get-value enumset s)
    (hashtable-ref
      (enum-universe-mapping (enum-info enumset)) s #f))

  (define (make-info symbols)
    (let ((ht (make-eq-hashtable)))
      (let f ((s symbols)(a '())(mask 1))
        (if (null? s)
            (values (reverse! a) ht (- mask 1))
            (let ((n (car s)))
              (if (symbol? n)
                  (if (hashtable-contains? ht n)
                      (f (cdr s) a mask)
                      (begin
                        (hashtable-set! ht n mask)
                        (f 
                          (cdr s) 
                          (cons n a) 
                          (bitwise-arithmetic-shift-left mask 1))))
                  (assertion-violation 'make-enumeration "not a symbol" n)))))))
              
  (define (string-join sep strings)
    (let f ((s strings)(a '()))
      (if (null? (cdr s))
          (apply string-append (reverse! (cons (car s) a)))
          (f (cdr s) (cons* sep (car s) a)))))
              
  (define (make-name symbols)
    (string->symbol (string-join " " (map symbol->string symbols))))

  (define (make-enumeration symbols)
    (let-values (((s mapping mask) (make-info symbols)))
      (make-enum mask
        (make-enum-universe (make-name s) s mapping mask))))

  (define/contract (enum-set-universe enumset:enum)
    (let ((ei (enum-info enumset)))
      (make-enum (enum-universe-value ei) ei)))

  (define/contract (enum-set-indexer enumset:enum)
    (lambda (symbol)
      (unless (symbol? symbol)
        (assertion-violation 'enum-set-indexer "not a symbol" symbol))
      (let ((v (get-value enumset symbol)))
        (if v
            (- (bitwise-length v) 1)
            #f))))

  (define/contract (enum-set-constructor enumset:enum)
    (lambda (symbols)
      (let f ((v 0)(s symbols))
        (if (null? s)
            (make-enum v (enum-info enumset))
            (let ((n (car s)))
              (if (symbol? n)
                  (let ((v* (get-value enumset n)))
                    (if v*
                        (f (bitwise-ior v v*) (cdr s))
                        (assertion-violation 'enum-set-constructor
                          "not a member of enum-set" n)))
                  (assertion-violation 'enum-set-constructor "not a symbol" n)))))))

  (define/contract (enum-set->list enumset:enum)
    (let ((value (enum-value enumset)))
      (let f ((s (get-symbols enumset))(l '()))
        (if (null? s)
            (reverse! l)
            (if (zero? (bitwise-and (get-value enumset (car s)) value))
                (f (cdr s) l)
                (f (cdr s) (cons (car s) l)))))))

  (define/contract (enum-set-member? symbol:symbol enumset:enum)
    (let ((v (get-value enumset symbol)))
      (if v
          (not (zero? (bitwise-and v (enum-value enumset))))
          #f)))

  (define/contract (enum-set-subset? enumset1:enum enumset2:enum)
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

  (define/contract (enum-set=? enumset1:enum enumset2:enum)
    (and
      (enum-set-subset? enumset1 enumset2)
      (enum-set-subset? enumset2 enumset1)))

  (define/contract (enum-set-union enumset1:enum enumset2:enum)
    (if (enum-type=? enumset1 enumset2)
        (make-enum
          (bitwise-ior (enum-value enumset1) (enum-value enumset2))
          (enum-info enumset1))
        #f))

  (define/contract (enum-set-intersection enumset1:enum enumset2:enum)
    (if (enum-type=? enumset1 enumset2)
        (make-enum
          (bitwise-and (enum-value enumset1) (enum-value enumset2))
          (enum-info enumset1))
        #f))

  (define/contract (enum-set-difference enumset1:enum enumset2:enum)
    (if (enum-type=? enumset1 enumset2)
        (make-enum
          (bitwise-and 
            (enum-value enumset1) 
            (bitwise-not (enum-value enumset2)))
          (enum-info enumset1))
        #f))

  (define/contract (enum-set-complement enumset:enum)
    (make-enum
      (bitwise-not (enum-value enumset))
      (enum-info enumset)))

  (define/contract (enum-set-projection enumset1:enum enumset2:enum)
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

  (add-record-printer! enum? 
    (lambda (x p wr)
      (fprintf p "#<enum-set ~a>" (enum-set->list x)))))      
