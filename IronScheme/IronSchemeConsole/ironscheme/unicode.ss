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
    ))

  (define-syntax make-string-ci-compare
    (syntax-rules ()
      [(_ cmp k)
        (lambda (a b . rest)
          (unless (string? a) (assertion-violation 'k "not a string" a))
          (unless (string? b) (assertion-violation 'k "not a string" b))  
          (for-each (lambda (x)
                      (unless (string? x) (assertion-violation 'k "not a string" x)))
                    rest)  
          (let f ((a a)(b b)(rest rest))                    
            (if (null? rest)
              (cmp (string-ci-compare a b) 0)
              (and 
                (cmp (string-compare a b) 0)
                (f b (car rest) (cdr rest))))))]))

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
          (unless (char? b) (assertion-violation 'k "not a char" b))  
          (for-each (lambda (x)
                      (unless (char? x) (assertion-violation 'k "not a char" x)))
                    rest)  
          (let f ((a a)(b b)(rest rest))                    
            (if (null? rest)
              (cmp (char->integer (char-upcase a)) (char->integer (char-upcase b)))
              (and 
                (cmp (char->integer (char-upcase a)) (char->integer (char-upcase b)))
                (f b (car rest) (cdr rest))))))]))
    
    
  (define char-ci=? (char-ci-compare = char-ci=?))
  (define char-ci<? (char-ci-compare < char-ci<?))
  (define char-ci>? (char-ci-compare > char-ci>?))
  (define char-ci<=? (char-ci-compare <= char-ci<=?))
  (define char-ci>=? (char-ci-compare >= char-ci>=?))
                
)
