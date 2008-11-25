(library (ironscheme unicode)
  (export
    char-upcase
    char-downcase
    char-titlecase
    char-foldcase
    
    char-ci=?
    char-ci<?
    char-ci>?
    char-ci<=?
    char-ci>=?
    
    char-alphabetic?
    char-numeric?
    char-whitespace?
    char-upper-case?
    char-lower-case?
    char-title-case?
    char-general-category
    
    string-upcase
    string-downcase
    string-titlecase
    string-foldcase
    
    string-ci=?
    string-ci<?
    string-ci>?
    string-ci<=?
    string-ci>=?
    
    string-normalize-nfd
    string-normalize-nfkd
    string-normalize-nfc
    string-normalize-nfkc)
    
  (import (except (ironscheme)
    char-ci=?
    char-ci<?
    char-ci>?
    char-ci<=?
    char-ci>=?
    string-ci=?
    string-ci<?
    string-ci>?
    string-ci<=?
    string-ci>=?
    string-normalize-nfd
    string-normalize-nfkd
    string-normalize-nfc
    string-normalize-nfkc    
    )
    (only (ironscheme core) string-normalize))

  (define-syntax make-string-ci-compare
    (syntax-rules ()
      [(_ cmp k)
        (lambda (a b . rest)
          (unless (string? a) (assertion-violation 'k "not a string" a))
          (for-all
            (lambda (x)
              (unless (string? x) (assertion-violation 'k "not a string" x))  
              (let ((r (cmp (string-ci-compare a x) 0)))
                (set! a x)
                r))
            (cons b rest)))]))

  (define string-ci=? (make-string-ci-compare = string-ci=?))
  (define string-ci<? (make-string-ci-compare < string-ci<?))
  (define string-ci>? (make-string-ci-compare > string-ci>?))
  (define string-ci<=? (make-string-ci-compare <= string-ci<=?))
  (define string-ci>=? (make-string-ci-compare >= string-ci>=?)) 
  
  (define-syntax char-ci-compare
    (syntax-rules ()
      [(_ cmp k)
        (lambda (a b . rest)
          (unless (char? a) (assertion-violation 'k "not a char" a))
          (for-all
            (lambda (x)
              (unless (char? x) (assertion-violation 'k "not a char" x))  
              (let ((r (cmp (char->integer (char-upcase a)) (char->integer (char-upcase x)))))
                (set! a x)
                r))
            (cons b rest)))]))    
    
  (define char-ci=? (char-ci-compare = char-ci=?))
  (define char-ci<? (char-ci-compare < char-ci<?))
  (define char-ci>? (char-ci-compare > char-ci>?))
  (define char-ci<=? (char-ci-compare <= char-ci<=?))
  (define char-ci>=? (char-ci-compare >= char-ci>=?))
  
  (define (string-normalize-nfc s)
    (string-normalize s 'formc))

  (define (string-normalize-nfd s)
    (string-normalize s 'formd))

  (define (string-normalize-nfkc s)
    (string-normalize s 'formkc))

  (define (string-normalize-nfkd s)
    (string-normalize s 'formkd))
                
)
