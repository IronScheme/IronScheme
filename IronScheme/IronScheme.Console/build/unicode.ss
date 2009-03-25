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
    
    char-upcase
    char-downcase
    char-titlecase
    char-foldcase
    
    char-alphabetic?
    char-numeric?
    char-whitespace?
    char-upper-case?
    char-lower-case?
    char-title-case?
    char-general-category
    
    string-ci-compare
    ;string-upcase
    ;string-downcase
    string-titlecase
    string-foldcase
    string-ci-compare
    )
    (ironscheme clr))
    
  (clr-using System.Globalization)   
    
  (define compare-info 
    (clr-prop-get CultureInfo CompareInfo
      (clr-static-prop-get CultureInfo InvariantCulture)))
      
  (define text-info 
    (clr-prop-get CultureInfo TextInfo
      (clr-static-prop-get CultureInfo InvariantCulture)))   
      
  (define (char-upcase chr)
    (unless (char? chr)
      (assertion-violation 'char-upcase "not a character" chr))
    (clr-static-call System.Char ToUpper chr))               
    
  (define (char-downcase chr)
    (unless (char? chr)
      (assertion-violation 'char-downcase "not a character" chr))
    (clr-static-call System.Char ToLower chr))               

  (define (char-titlecase chr)
    (unless (char? chr)
      (assertion-violation 'char-titlecase "not a character" chr))
    (string-ref (clr-call TextInfo 
                          ToTitleCase 
                          text-info 
                          (->string (string chr)))
                0))               

  (define (char-foldcase chr)
    (unless (char? chr)
      (assertion-violation 'char-foldcase "not a character" chr))
    (clr-static-call System.Char 
                     ToLowerInvariant 
                     (clr-static-call System.Char 
                                      ToUpperInvariant 
                                      chr)))
    
  (define (char-alphabetic? chr)
    (unless (char? chr)
      (assertion-violation 'char-alphabetic? "not a character" chr))
    (clr-static-call System.Char "IsLetter(Char)" chr))               

  (define (char-numeric? chr)
    (unless (char? chr)
      (assertion-violation 'char-numeric? "not a character" chr))
    (clr-static-call System.Char "IsDigit(Char)" chr))               

  (define (char-whitespace? chr)
    (unless (char? chr)
      (assertion-violation 'char-whitespace? "not a character" chr))
    (clr-static-call System.Char "IsWhiteSpace(Char)" chr))               

  (define (char-upper-case? chr)
    (unless (char? chr)
      (assertion-violation 'char-upper-case? "not a character" chr))
    (clr-static-call System.Char "IsUpper(Char)" chr)) 
    
  (define (char-lower-case? chr)
    (unless (char? chr)
      (assertion-violation 'char-lower-case? "not a character" chr))
    (clr-static-call System.Char "IsLower(Char)" chr)) 
    
  (define (char-title-case? chr)
    (unless (char? chr)
      (assertion-violation 'char-title-case? "not a character" chr))
    (case chr
      [(#\I #\A) #f]
      [else
        (eqv? chr (char-titlecase chr))]))
        
  (define (char-general-category chr)        
    (unless (char? chr)
      (assertion-violation 'char-general-category "not a character" chr))
    (case (clr-static-call System.Char "GetUnicodeCategory(Char)" chr)
      [(closepunctuation)          'Pe]
      [(connectorpunctuation)      'Pc]
      [(control)                   'Cc]
      [(currencysymbol)            'Sc]
      [(dashpunctuation)           'Pd]
      [(decimaldigitnumber)        'Nd]
      [(enclosingmark)             'Me]
      [(finalquotepunctuation)     'Pf]
      [(format)                    'Cf]
      [(initialquotepunctuation)   'Pi]
      [(letternumber)              'Nl]
      [(lineseparator)             'Zl]
      [(lowercaseletter)           'Ll]
      [(mathsymbol)                'Sm]
      [(modifierletter)            'Lm]
      [(modifiersymbol)            'Sk]
      [(nonspacingmark)            'Mn]
      [(openpunctuation)           'Ps]
      [(otherletter)               'Lo]
      [(othernotassigned)          'Cn]
      [(othernumber)               'No]
      [(otherpunctuation)          'Po]
      [(othersymbol)               'So]
      [(paragraphseparator)        'Zp]
      [(privateuse)                'Co]
      [(spaceseparator)            'Zs]
      [(spacingcombiningmark)      'Mc]
      [(surrogate)                 'Cs]
      [(titlecaseletter)           'Lt]
      [(uppercaseletter)           'Lu]
      [else #f]))
      
  (define (clr-string? obj)
    (clr-is System.String obj))  

  (define (->string str)
    (if (clr-string? str)
        str
        (clr-call Object ToString str)))      
      
  (define-syntax $string-ci-compare
    (syntax-rules ()
      [(_ a b)
        (clr-call CompareInfo 
                  "Compare(String,String,CompareOptions)" 
                  compare-info 
                  (->string a) 
                  (->string b) 
                  'IgnoreCase)]))
        
  (define (string-ci-compare a b)
    (unless (string? a) (assertion-violation 'string-ci-compare "not a string" a))
    (unless (string? b) (assertion-violation 'string-ci-compare "not a string" b))
    ($string-ci-compare a b))
    
  (define-syntax make-string-ci-compare
    (syntax-rules ()
      [(_ cmp k)
        (lambda (a b . rest)
          (unless (string? a) (assertion-violation 'k "not a string" a))
          (for-all
            (lambda (x)
              (unless (string? x) (assertion-violation 'k "not a string" x))  
              (let ((r (cmp ($string-ci-compare a x) 0)))
                (set! a x)
                r))
            (cons b rest)))]))

  (define string-ci=? (make-string-ci-compare fx=? string-ci=?))
  (define string-ci<? (make-string-ci-compare fx<? string-ci<?))
  (define string-ci>? (make-string-ci-compare fx>? string-ci>?))
  (define string-ci<=? (make-string-ci-compare fx<=? string-ci<=?))
  (define string-ci>=? (make-string-ci-compare fx>=? string-ci>=?)) 
  
  
  (define (string-titlecase str)
    (unless (string? str) 
      (assertion-violation 'string-titlecase "not a string" str))
    (clr-call TextInfo ToTitleCase text-info (string-downcase str)))
    
  (define (string-foldcase str)
    (unless (string? str) 
      (assertion-violation 'string-foldcase "not a string" str))
    (clr-call System.String ToLowerInvariant (string-upcase str)))
    
  
  
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
    
  (define char-ci=? (char-ci-compare fx=? char-ci=?))
  (define char-ci<? (char-ci-compare fx<? char-ci<?))
  (define char-ci>? (char-ci-compare fx>? char-ci>?))
  (define char-ci<=? (char-ci-compare fx<=? char-ci<=?))
  (define char-ci>=? (char-ci-compare fx>=? char-ci>=?))
  

          
  (define-syntax string-normalize
    (syntax-rules ()
      [(_ str form)
        (if (string? str)
            (clr-call System.String Normalize (->string str) 'form)
            (assertion-violation 'string-normalize "not a string" str))]))
  
  (define (string-normalize-nfc s)
    (string-normalize s formc))

  (define (string-normalize-nfd s)
    (string-normalize s formd))

  (define (string-normalize-nfkc s)
    (string-normalize s formkc))

  (define (string-normalize-nfkd s)
    (string-normalize s formkd))
    
                
)
