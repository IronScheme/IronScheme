#| ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 2007,2008,2009
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************|#

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
    string-upcase
    ;string-downcase
    string-titlecase
    string-foldcase
    string-ci-compare
    )
    (ironscheme clr)
    (ironscheme contracts))
    
  (clr-using System.Globalization)   
    
  (define compare-info 
    (clr-prop-get CultureInfo CompareInfo
      (clr-static-prop-get CultureInfo InvariantCulture)))
      
  (define text-info 
    (clr-prop-get CultureInfo TextInfo
      (clr-static-prop-get CultureInfo InvariantCulture)))   
      
  (define/contract (char-upcase chr:char)
    (clr-static-call Char ToUpper chr))               
    
  (define/contract (char-downcase chr:char)
    (clr-static-call Char ToLower chr))               

  (define/contract (char-titlecase chr:char)
    (string-ref (clr-call TextInfo 
                          ToTitleCase 
                          text-info 
                          (->string (string chr)))
                0))               

  (define/contract (char-foldcase chr:char)
    (clr-static-call Char 
                     ToLowerInvariant 
                     (clr-static-call Char 
                                      ToUpperInvariant 
                                      chr)))
    
  (define/contract (char-alphabetic? chr:char)
    (clr-static-call Char "IsLetter(Char)" chr))               

  (define/contract (char-numeric? chr:char)
    (clr-static-call Char "IsDigit(Char)" chr))               

  (define/contract (char-whitespace? chr:char)
    (clr-static-call Char "IsWhiteSpace(Char)" chr))               

  (define/contract (char-upper-case? chr:char)
    (clr-static-call Char "IsUpper(Char)" chr)) 
    
  (define/contract (char-lower-case? chr:char)
    (clr-static-call Char "IsLower(Char)" chr)) 
    
  (define/contract (char-title-case? chr:char)
    (case chr
      [(#\I #\A) #f]
      [else
        (eqv? chr (char-titlecase chr))]))
        
  (define/contract (char-general-category chr:char)
    (case (clr-static-call Char "GetUnicodeCategory(Char)" chr)
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
    (clr-is String obj))  

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
        
  (define/contract (string-ci-compare a:string b:string)
    ($string-ci-compare a b))
    
  (define-syntax define-string-ci-compare
    (syntax-rules ()
      [(_ name cmp)
        (define/contract name
          (case-lambda
            [(a:string b:string) 
              (cmp ($string-ci-compare a b) 0)]
            [(a:string b . rest)
              (for-all
                (lambda (x)
                  (unless (string? x) (assertion-violation 'name "not a string" x))  
                  (let ((r (cmp ($string-ci-compare a x) 0)))
                    (set! a x)
                    r))
                (cons b rest))]))]))

  (define-string-ci-compare string-ci=? fx=?)
  (define-string-ci-compare string-ci<? fx<?)
  (define-string-ci-compare string-ci>? fx>?)
  (define-string-ci-compare string-ci<=? fx<=?)
  (define-string-ci-compare string-ci>=? fx>=?)
  
  (define/contract (string-titlecase str:string)
    (clr-call TextInfo ToTitleCase text-info (string-downcase str)))
    
  (define/contract (string-foldcase str:string)
    (clr-call String ToLowerInvariant (string-upcase str)))
  
  (define-syntax define-char-ci-compare
    (syntax-rules ()
      [(_ name cmp)
        (define/contract name
          (case-lambda
            [(a:char b:char)
              (cmp (char->integer (char-upcase a)) (char->integer (char-upcase b)))]
            [(a:char b . rest)
              (for-all
                (lambda (x)
                  (unless (char? x) (assertion-violation 'k "not a char" x))  
                  (let ((r (cmp (char->integer (char-upcase a)) (char->integer (char-upcase x)))))
                    (set! a x)
                    r))
                (cons b rest))]))]))    
    
  (define-char-ci-compare char-ci=? fx=?)
  (define-char-ci-compare char-ci<? fx<?)
  (define-char-ci-compare char-ci>? fx>?)
  (define-char-ci-compare char-ci<=? fx<=?)
  (define-char-ci-compare char-ci>=? fx>=?)
  
  (define/contract (string-upcase str:string)
    (clr-call String Replace 
      (clr-call String ToUpper (->string str))
      "ß" 
      "SS"))

  (define-syntax string-normalize
    (syntax-rules ()
      [(_ str form)
        (if (string? str)
            (clr-call String Normalize (->string str) 'form)
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
