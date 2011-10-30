#| License
Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#

(library (ironscheme unicode)
  (export
    char-upcase
    char-downcase
    char-titlecase
    char-foldcase
    
    char=?
    char<?
    char>?
    char<=?
    char>=?    
    
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
    
    string=?
    string<?
    string>?
    string<=?
    string>=?
    
    string-ci=?
    string-ci<?
    string-ci>?
    string-ci<=?
    string-ci>=?
    
    string-compare
    string-ci-compare
    
    string-normalize-nfd
    string-normalize-nfkd
    string-normalize-nfc
    string-normalize-nfkc)
    
  (import (except (ironscheme)
    char=?
    char<?
    char>?
    char<=?
    char>=?    
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
    string=?
    string<?
    string>?
    string<=?
    string>=?
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
    string-downcase
    string-titlecase
    string-foldcase
    string-compare
    string-ci-compare
    char->integer
    )
    (ironscheme clr)
    (ironscheme unsafe)
    (ironscheme contracts))
    
  (clr-using System.Globalization) 
  
  (define char->integer
    (typed-lambda (chr)
      ((Char) Int32)
      chr))
  
  (define culture-info  
    (clr-static-prop-get CultureInfo InvariantCulture))
    
  (define compare-info 
    (clr-prop-get CultureInfo CompareInfo culture-info))
      
  (define text-info 
    (clr-prop-get CultureInfo TextInfo culture-info))
      
  (define/contract (char-upcase chr:char)
    (clr-static-call Char ToUpper chr))               
    
  (define/contract (char-downcase chr:char)
    (clr-static-call Char ToLower chr))               

  (define/contract (char-titlecase chr:char)
    (if (char-title-case? chr)
        chr
        (char-upcase chr)))

  (define/contract (char-foldcase chr:char)
    (case chr
      [(#\x130 #\x131) chr]
      [else
        (char-downcase (char-upcase chr))]))
    
  (define/contract (char-alphabetic? chr:char)
    (clr-static-call Char (IsLetter Char) chr))               

  (define/contract (char-numeric? chr:char)
    (clr-static-call Char (IsDigit Char) chr))               

  (define/contract (char-whitespace? chr:char)
    (clr-static-call Char (IsWhiteSpace Char) chr))               

  (define/contract (char-upper-case? chr:char)
    (eq? 'Lu (char-general-category chr)))
    
  (define/contract (char-lower-case? chr:char)
    (eq? 'Ll (char-general-category chr)))
    
  (define/contract (char-title-case? chr:char)
    (eq? 'Lt (char-general-category chr)))
        
  (define/contract (char-general-category chr:char)
    (case (clr-static-call Char (GetUnicodeCategory Char) chr)
      [(ClosePunctuation)          'Pe]
      [(ConnectorPunctuation)      'Pc]
      [(Control)                   'Cc]
      [(CurrencySymbol)            'Sc]
      [(DashPunctuation)           'Pd]
      [(DecimalDigitNumber)        'Nd]
      [(EnclosingMark)             'Me]
      [(FinalQuotePunctuation)     'Pf]
      [(Format)                    'Cf]
      [(InitialQuotePunctuation)   'Pi]
      [(LetterNumber)              'Nl]
      [(LineSeparator)             'Zl]
      [(LowercaseLetter)           'Ll]
      [(MathSymbol)                'Sm]
      [(ModifierLetter)            'Lm]
      [(ModifierSymbol)            'Sk]
      [(NonSpacingMark)            'Mn]
      [(OpenPunctuation)           'Ps]
      [(OtherLetter)               'Lo]
      [(OtherNotAssigned)          'Cn]
      [(OtherNumber)               'No]
      [(OtherPunctuation)          'Po]
      [(OtherSymbol)               'So]
      [(ParagraphSeparator)        'Zp]
      [(PrivateUse)                'Co]
      [(SpaceSeparator)            'Zs]
      [(SpacingCombiningMark)      'Mc]
      [(Surrogate)                 'Cs]
      [(TitlecaseLetter)           'Lt]
      [(UppercaseLetter)           'Lu]
      [else #f]))
      
  (define (clr-string? obj)
    (clr-is String obj))  

  (define ->string
    (typed-lambda (str)
      ((Object) String)    
      (if (clr-string? str)
          str
          (clr-call Object ToString str))))   
        
  (define/contract (string-compare a:string b:string)
    ($string-compare a b))
                     
  (define-syntax $string-compare
    (syntax-rules ()
      [(_ a b)
        (clr-call CompareInfo 
                  (Compare String String CompareOptions)
                  compare-info 
                  (->string a) 
                  (->string b) 
                  'Ordinal)]))                       
                       
  (define-syntax define-string-compare
    (syntax-rules ()
      [(_ name cmp)
        (define/contract name
          (case-lambda
            [(a:string b:string) 
              (cmp ($string-compare a b) 0)]
            [(a:string b . rest)
              (for-all
                (lambda (x)
                  (unless (string? x) (assertion-violation 'name "not a string" x))  
                  (let ((r (cmp ($string-compare a x) 0)))
                    (set! a x)
                    r))
                (cons b rest))]))]))                       

  (define-string-compare string=? $fx=?)
  (define-string-compare string<? $fx<?)
  (define-string-compare string>? $fx>?)
  (define-string-compare string<=? $fx<=?)
  (define-string-compare string>=? $fx>=?)           
      
  (define-syntax $string-ci-compare
    (syntax-rules ()
      [(_ a b)
        ($string-compare (string-foldcase a) 
                         (string-foldcase b))]))
        
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

  (define-string-ci-compare string-ci=? $fx=?)
  (define-string-ci-compare string-ci<? $fx<?)
  (define-string-ci-compare string-ci>? $fx>?)
  (define-string-ci-compare string-ci<=? $fx<=?)
  (define-string-ci-compare string-ci>=? $fx>=?)
        
  (define-syntax define-char-compare
    (syntax-rules ()
      [(_ name cmp)
        (define/contract name
          (case-lambda
            [(a:char b:char)
              (cmp (char->integer a) (char->integer b))]
            [(a:char b . rest)
              (for-all
                (lambda (x)
                  (unless (char? x) (assertion-violation 'name "not a char" x))  
                  (let ((r (cmp (char->integer a) (char->integer x))))
                    (set! a x)
                    r))
                (cons b rest))]))]))         
  
  (define-char-compare char=? $fx=?)
  (define-char-compare char<? $fx<?)
  (define-char-compare char>? $fx>?)
  (define-char-compare char<=? $fx<=?)
  (define-char-compare char>=? $fx>=?) 
  
  (define-syntax define-char-ci-compare
    (syntax-rules ()
      [(_ name cmp)
        (define/contract name
          (case-lambda
            [(a:char b:char)
              (cmp (char->integer (char-foldcase a)) (char->integer (char-foldcase b)))]
            [(a:char b . rest)
              (for-all
                (lambda (x)
                  (unless (char? x) (assertion-violation 'name "not a char" x))  
                  (let ((r (cmp (char->integer (char-foldcase a)) (char->integer (char-foldcase x)))))
                    (set! a x)
                    r))
                (cons b rest))]))]))    
    
  (define-char-ci-compare char-ci=? $fx=?)
  (define-char-ci-compare char-ci<? $fx<?)
  (define-char-ci-compare char-ci>? $fx>?)
  (define-char-ci-compare char-ci<=? $fx<=?)
  (define-char-ci-compare char-ci>=? $fx>=?)
  
  (define (string-fold proc str)
    (if (fxzero? (string-length str))
        ""
        (let* ((chars (string->list str))
               (before (cons #f (reverse (cdr (reverse chars)))))
               (after (append (cdr chars) (list #f))))
          (apply string-append (map proc chars before after)))))
      
  (define (string-fold-fast proc str)
    (apply string-append (map proc (string->list str))))
      
  (define/contract (string-downcase str:string)
    (string-fold 
      (lambda (c b a)
        (let ((sh (hashtable-ref special-case-map c #f)))
          (if (and sh (sh b a))
              (let ((uc (hashtable-ref lower-case-map c #f)))
                (or uc
                    (string (char-downcase c))))
               (string (char-downcase c)))))
      str))
  
  (define/contract (string-upcase str:string)
    (string-fold 
      (lambda (c b a)
        (let ((uc (hashtable-ref upper-case-map c #f)))
          (or uc
              (string (char-upcase c)))))
      str))
      
  (define/contract (string-titlecase str:string)
    (string-fold 
      (lambda (c b a)
        (if (or (not b) (char-whitespace? b))
            (let ((uc (hashtable-ref title-case-map c #f)))
              (or uc
                  (string (char-titlecase c))))
            (string (char-downcase c))))
      str))
    
  (define/contract (string-foldcase str:string)
    (string-fold-fast
      (lambda (c)
        (let ((uc (hashtable-ref case-fold-map c #f)))
          (or uc
              (string (char-foldcase c)))))
      str))    

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
    
  (define lower-case-map (make-eqv-hashtable))
  (define title-case-map (make-eqv-hashtable))
  (define upper-case-map (make-eqv-hashtable))
  (define special-case-map (make-eqv-hashtable))
  (define case-fold-map (make-eqv-hashtable))
  
  (define (make-fold-case key val)
    (hashtable-set! case-fold-map key val))
  
  (define make-case
    (case-lambda
      [(lower title upper)
        (let ((key (string-ref lower 0))) ; all input 1 char long
          (hashtable-set! title-case-map key title)
          (hashtable-set! upper-case-map key upper))]
      [(key lower title upper)
        (hashtable-set! lower-case-map key lower)
        (hashtable-set! title-case-map key title)
        (hashtable-set! upper-case-map key upper)]
      [(key lower title upper pred)
        (hashtable-set! lower-case-map key lower)
        (hashtable-set! title-case-map key title)
        (hashtable-set! upper-case-map key upper)
        (hashtable-set! special-case-map key pred)]))
  
  (define (initialize-case-maps)      
    (make-case "\x00DF;" "\x0053;\x0073;" "\x0053;\x0053;" )
    (make-case #\x0130 "\x0069;\x0307;" "\x0130;" "\x0130;" ) 
    (make-case "\xFB00;" "\x0046;\x0066;" "\x0046;\x0046;" ) 
    (make-case "\xFB01;" "\x0046;\x0069;" "\x0046;\x0049;" ) 
    (make-case "\xFB02;" "\x0046;\x006C;" "\x0046;\x004C;" ) 
    (make-case "\xFB03;" "\x0046;\x0066;\x0069;" "\x0046;\x0046;\x0049;" ) 
    (make-case "\xFB04;" "\x0046;\x0066;\x006C;" "\x0046;\x0046;\x004C;" ) 
    (make-case "\xFB05;" "\x0053;\x0074;" "\x0053;\x0054;" ) 
    (make-case "\xFB06;" "\x0053;\x0074;" "\x0053;\x0054;" ) 
    (make-case "\x0587;" "\x0535;\x0582;" "\x0535;\x0552;" ) 
    (make-case "\xFB13;" "\x0544;\x0576;" "\x0544;\x0546;" ) 
    (make-case "\xFB14;" "\x0544;\x0565;" "\x0544;\x0535;" ) 
    (make-case "\xFB15;" "\x0544;\x056B;" "\x0544;\x053B;" ) 
    (make-case "\xFB16;" "\x054E;\x0576;" "\x054E;\x0546;" ) 
    (make-case "\xFB17;" "\x0544;\x056D;" "\x0544;\x053D;" ) 
    (make-case "\x0149;" "\x02BC;\x004E;" "\x02BC;\x004E;" ) 
    (make-case "\x0390;" "\x0399;\x0308;\x0301;" "\x0399;\x0308;\x0301;" ) 
    (make-case "\x03B0;" "\x03A5;\x0308;\x0301;" "\x03A5;\x0308;\x0301;" ) 
    (make-case "\x01F0;" "\x004A;\x030C;" "\x004A;\x030C;" ) 
    (make-case "\x1E96;" "\x0048;\x0331;" "\x0048;\x0331;" ) 
    (make-case "\x1E97;" "\x0054;\x0308;" "\x0054;\x0308;" ) 
    (make-case "\x1E98;" "\x0057;\x030A;" "\x0057;\x030A;" ) 
    (make-case "\x1E99;" "\x0059;\x030A;" "\x0059;\x030A;" ) 
    (make-case "\x1E9A;" "\x0041;\x02BE;" "\x0041;\x02BE;" ) 
    (make-case "\x1F50;" "\x03A5;\x0313;" "\x03A5;\x0313;" ) 
    (make-case "\x1F52;" "\x03A5;\x0313;\x0300;" "\x03A5;\x0313;\x0300;" ) 
    (make-case "\x1F54;" "\x03A5;\x0313;\x0301;" "\x03A5;\x0313;\x0301;" ) 
    (make-case "\x1F56;" "\x03A5;\x0313;\x0342;" "\x03A5;\x0313;\x0342;" ) 
    (make-case "\x1FB6;" "\x0391;\x0342;" "\x0391;\x0342;" ) 
    (make-case "\x1FC6;" "\x0397;\x0342;" "\x0397;\x0342;" ) 
    (make-case "\x1FD2;" "\x0399;\x0308;\x0300;" "\x0399;\x0308;\x0300;" ) 
    (make-case "\x1FD3;" "\x0399;\x0308;\x0301;" "\x0399;\x0308;\x0301;" ) 
    (make-case "\x1FD6;" "\x0399;\x0342;" "\x0399;\x0342;" ) 
    (make-case "\x1FD7;" "\x0399;\x0308;\x0342;" "\x0399;\x0308;\x0342;" ) 
    (make-case "\x1FE2;" "\x03A5;\x0308;\x0300;" "\x03A5;\x0308;\x0300;" ) 
    (make-case "\x1FE3;" "\x03A5;\x0308;\x0301;" "\x03A5;\x0308;\x0301;" ) 
    (make-case "\x1FE4;" "\x03A1;\x0313;" "\x03A1;\x0313;" ) 
    (make-case "\x1FE6;" "\x03A5;\x0342;" "\x03A5;\x0342;" ) 
    (make-case "\x1FE7;" "\x03A5;\x0308;\x0342;" "\x03A5;\x0308;\x0342;" ) 
    (make-case "\x1FF6;" "\x03A9;\x0342;" "\x03A9;\x0342;" ) 
    (make-case "\x1F80;" "\x1F88;" "\x1F08;\x0399;" ) 
    (make-case "\x1F81;" "\x1F89;" "\x1F09;\x0399;" ) 
    (make-case "\x1F82;" "\x1F8A;" "\x1F0A;\x0399;" ) 
    (make-case "\x1F83;" "\x1F8B;" "\x1F0B;\x0399;" ) 
    (make-case "\x1F84;" "\x1F8C;" "\x1F0C;\x0399;" ) 
    (make-case "\x1F85;" "\x1F8D;" "\x1F0D;\x0399;" ) 
    (make-case "\x1F86;" "\x1F8E;" "\x1F0E;\x0399;" ) 
    (make-case "\x1F87;" "\x1F8F;" "\x1F0F;\x0399;" ) 
    (make-case "\x1F80;" "\x1F88;" "\x1F08;\x0399;" ) 
    (make-case "\x1F81;" "\x1F89;" "\x1F09;\x0399;" ) 
    (make-case "\x1F82;" "\x1F8A;" "\x1F0A;\x0399;" ) 
    (make-case "\x1F83;" "\x1F8B;" "\x1F0B;\x0399;" ) 
    (make-case "\x1F84;" "\x1F8C;" "\x1F0C;\x0399;" ) 
    (make-case "\x1F85;" "\x1F8D;" "\x1F0D;\x0399;" ) 
    (make-case "\x1F86;" "\x1F8E;" "\x1F0E;\x0399;" ) 
    (make-case "\x1F87;" "\x1F8F;" "\x1F0F;\x0399;" ) 
    (make-case "\x1F90;" "\x1F98;" "\x1F28;\x0399;" ) 
    (make-case "\x1F91;" "\x1F99;" "\x1F29;\x0399;" ) 
    (make-case "\x1F92;" "\x1F9A;" "\x1F2A;\x0399;" ) 
    (make-case "\x1F93;" "\x1F9B;" "\x1F2B;\x0399;" ) 
    (make-case "\x1F94;" "\x1F9C;" "\x1F2C;\x0399;" ) 
    (make-case "\x1F95;" "\x1F9D;" "\x1F2D;\x0399;" ) 
    (make-case "\x1F96;" "\x1F9E;" "\x1F2E;\x0399;" ) 
    (make-case "\x1F97;" "\x1F9F;" "\x1F2F;\x0399;" ) 
    (make-case "\x1F90;" "\x1F98;" "\x1F28;\x0399;" ) 
    (make-case "\x1F91;" "\x1F99;" "\x1F29;\x0399;" ) 
    (make-case "\x1F92;" "\x1F9A;" "\x1F2A;\x0399;" ) 
    (make-case "\x1F93;" "\x1F9B;" "\x1F2B;\x0399;" ) 
    (make-case "\x1F94;" "\x1F9C;" "\x1F2C;\x0399;" ) 
    (make-case "\x1F95;" "\x1F9D;" "\x1F2D;\x0399;" ) 
    (make-case "\x1F96;" "\x1F9E;" "\x1F2E;\x0399;" ) 
    (make-case "\x1F97;" "\x1F9F;" "\x1F2F;\x0399;" ) 
    (make-case "\x1FA0;" "\x1FA8;" "\x1F68;\x0399;" ) 
    (make-case "\x1FA1;" "\x1FA9;" "\x1F69;\x0399;" ) 
    (make-case "\x1FA2;" "\x1FAA;" "\x1F6A;\x0399;" ) 
    (make-case "\x1FA3;" "\x1FAB;" "\x1F6B;\x0399;" ) 
    (make-case "\x1FA4;" "\x1FAC;" "\x1F6C;\x0399;" ) 
    (make-case "\x1FA5;" "\x1FAD;" "\x1F6D;\x0399;" ) 
    (make-case "\x1FA6;" "\x1FAE;" "\x1F6E;\x0399;" ) 
    (make-case "\x1FA7;" "\x1FAF;" "\x1F6F;\x0399;" ) 
    (make-case "\x1FA0;" "\x1FA8;" "\x1F68;\x0399;" ) 
    (make-case "\x1FA1;" "\x1FA9;" "\x1F69;\x0399;" ) 
    (make-case "\x1FA2;" "\x1FAA;" "\x1F6A;\x0399;" ) 
    (make-case "\x1FA3;" "\x1FAB;" "\x1F6B;\x0399;" ) 
    (make-case "\x1FA4;" "\x1FAC;" "\x1F6C;\x0399;" ) 
    (make-case "\x1FA5;" "\x1FAD;" "\x1F6D;\x0399;" ) 
    (make-case "\x1FA6;" "\x1FAE;" "\x1F6E;\x0399;" ) 
    (make-case "\x1FA7;" "\x1FAF;" "\x1F6F;\x0399;" ) 
    (make-case "\x1FB3;" "\x1FBC;" "\x0391;\x0399;" ) 
    (make-case "\x1FB3;" "\x1FBC;" "\x0391;\x0399;" ) 
    (make-case "\x1FC3;" "\x1FCC;" "\x0397;\x0399;" ) 
    (make-case "\x1FC3;" "\x1FCC;" "\x0397;\x0399;" ) 
    (make-case "\x1FF3;" "\x1FFC;" "\x03A9;\x0399;" ) 
    (make-case "\x1FF3;" "\x1FFC;" "\x03A9;\x0399;" ) 
    (make-case "\x1FB2;" "\x1FBA;\x0345;" "\x1FBA;\x0399;" ) 
    (make-case "\x1FB4;" "\x0386;\x0345;" "\x0386;\x0399;" ) 
    (make-case "\x1FC2;" "\x1FCA;\x0345;" "\x1FCA;\x0399;" ) 
    (make-case "\x1FC4;" "\x0389;\x0345;" "\x0389;\x0399;" ) 
    (make-case "\x1FF2;" "\x1FFA;\x0345;" "\x1FFA;\x0399;" ) 
    (make-case "\x1FF4;" "\x038F;\x0345;" "\x038F;\x0399;" ) 
    (make-case "\x1FB7;" "\x0391;\x0342;\x0345;" "\x0391;\x0342;\x0399;" ) 
    (make-case "\x1FC7;" "\x0397;\x0342;\x0345;" "\x0397;\x0342;\x0399;" ) 
    (make-case "\x1FF7;" "\x03A9;\x0342;\x0345;" "\x03A9;\x0342;\x0399;" ) 
    (make-case #\x03A3 "\x03C2;" "\x03A3;" "\x03A3;" 
      (lambda (b a) ; almost correct
        (if (or (not b) (char-whitespace? b))
            #f
            (or (not a) (char-whitespace? a)))))) 
            
  (define (initialize-case-folding)
    (make-fold-case #\x00B5 "\x03BC;")
    (make-fold-case #\x00DF "\x0073;\x0073;")
    (make-fold-case #\x0130 "\x0069;\x0307;")
    (make-fold-case #\x0149 "\x02BC;\x006E;")
    (make-fold-case #\x017F "\x0073;")
    (make-fold-case #\x01A6 "\x0280;")
    (make-fold-case #\x01F0 "\x006A;\x030C;")
    (make-fold-case #\x01F6 "\x0195;")
    (make-fold-case #\x01F7 "\x01BF;")
    (make-fold-case #\x01F8 "\x01F9;")
    (make-fold-case #\x0218 "\x0219;")
    (make-fold-case #\x021A "\x021B;")
    (make-fold-case #\x021C "\x021D;")
    (make-fold-case #\x021E "\x021F;")
    (make-fold-case #\x0220 "\x019E;")
    (make-fold-case #\x0222 "\x0223;")
    (make-fold-case #\x0224 "\x0225;")
    (make-fold-case #\x0226 "\x0227;")
    (make-fold-case #\x0228 "\x0229;")
    (make-fold-case #\x022A "\x022B;")
    (make-fold-case #\x022C "\x022D;")
    (make-fold-case #\x022E "\x022F;")
    (make-fold-case #\x0230 "\x0231;")
    (make-fold-case #\x0232 "\x0233;")
    (make-fold-case #\x023A "\x2C65;")
    (make-fold-case #\x023B "\x023C;")
    (make-fold-case #\x023D "\x019A;")
    (make-fold-case #\x023E "\x2C66;")
    (make-fold-case #\x0241 "\x0242;")
    (make-fold-case #\x0243 "\x0180;")
    (make-fold-case #\x0244 "\x0289;")
    (make-fold-case #\x0245 "\x028C;")
    (make-fold-case #\x0246 "\x0247;")
    (make-fold-case #\x0248 "\x0249;")
    (make-fold-case #\x024A "\x024B;")
    (make-fold-case #\x024C "\x024D;")
    (make-fold-case #\x024E "\x024F;")
    (make-fold-case #\x0345 "\x03B9;")
    (make-fold-case #\x0370 "\x0371;")
    (make-fold-case #\x0372 "\x0373;")
    (make-fold-case #\x0376 "\x0377;")
    (make-fold-case #\x0390 "\x03B9;\x0308;\x0301;")
    (make-fold-case #\x03B0 "\x03C5;\x0308;\x0301;")
    (make-fold-case #\x03CF "\x03D7;")
    (make-fold-case #\x03D8 "\x03D9;")
    (make-fold-case #\x03DA "\x03DB;")
    (make-fold-case #\x03DC "\x03DD;")
    (make-fold-case #\x03DE "\x03DF;")
    (make-fold-case #\x03E0 "\x03E1;")
    (make-fold-case #\x03F4 "\x03B8;")
    (make-fold-case #\x03F5 "\x03B5;")
    (make-fold-case #\x03F7 "\x03F8;")
    (make-fold-case #\x03F9 "\x03F2;")
    (make-fold-case #\x03FA "\x03FB;")
    (make-fold-case #\x03FD "\x037B;")
    (make-fold-case #\x03FE "\x037C;")
    (make-fold-case #\x03FF "\x037D;")
    (make-fold-case #\x048A "\x048B;")
    (make-fold-case #\x048C "\x048D;")
    (make-fold-case #\x048E "\x048F;")
    (make-fold-case #\x04C0 "\x04CF;")
    (make-fold-case #\x04C5 "\x04C6;")
    (make-fold-case #\x04C9 "\x04CA;")
    (make-fold-case #\x04CD "\x04CE;")
    (make-fold-case #\x04EC "\x04ED;")
    (make-fold-case #\x04FA "\x04FB;")
    (make-fold-case #\x04FC "\x04FD;")
    (make-fold-case #\x04FE "\x04FF;")
    (make-fold-case #\x0500 "\x0501;")
    (make-fold-case #\x0502 "\x0503;")
    (make-fold-case #\x0504 "\x0505;")
    (make-fold-case #\x0506 "\x0507;")
    (make-fold-case #\x0508 "\x0509;")
    (make-fold-case #\x050A "\x050B;")
    (make-fold-case #\x050C "\x050D;")
    (make-fold-case #\x050E "\x050F;")
    (make-fold-case #\x0510 "\x0511;")
    (make-fold-case #\x0512 "\x0513;")
    (make-fold-case #\x0514 "\x0515;")
    (make-fold-case #\x0516 "\x0517;")
    (make-fold-case #\x0518 "\x0519;")
    (make-fold-case #\x051A "\x051B;")
    (make-fold-case #\x051C "\x051D;")
    (make-fold-case #\x051E "\x051F;")
    (make-fold-case #\x0520 "\x0521;")
    (make-fold-case #\x0522 "\x0523;")
    (make-fold-case #\x0524 "\x0525;")
    (make-fold-case #\x0587 "\x0565;\x0582;")
    (make-fold-case #\x10A0 "\x2D00;")
    (make-fold-case #\x10A1 "\x2D01;")
    (make-fold-case #\x10A2 "\x2D02;")
    (make-fold-case #\x10A3 "\x2D03;")
    (make-fold-case #\x10A4 "\x2D04;")
    (make-fold-case #\x10A5 "\x2D05;")
    (make-fold-case #\x10A6 "\x2D06;")
    (make-fold-case #\x10A7 "\x2D07;")
    (make-fold-case #\x10A8 "\x2D08;")
    (make-fold-case #\x10A9 "\x2D09;")
    (make-fold-case #\x10AA "\x2D0A;")
    (make-fold-case #\x10AB "\x2D0B;")
    (make-fold-case #\x10AC "\x2D0C;")
    (make-fold-case #\x10AD "\x2D0D;")
    (make-fold-case #\x10AE "\x2D0E;")
    (make-fold-case #\x10AF "\x2D0F;")
    (make-fold-case #\x10B0 "\x2D10;")
    (make-fold-case #\x10B1 "\x2D11;")
    (make-fold-case #\x10B2 "\x2D12;")
    (make-fold-case #\x10B3 "\x2D13;")
    (make-fold-case #\x10B4 "\x2D14;")
    (make-fold-case #\x10B5 "\x2D15;")
    (make-fold-case #\x10B6 "\x2D16;")
    (make-fold-case #\x10B7 "\x2D17;")
    (make-fold-case #\x10B8 "\x2D18;")
    (make-fold-case #\x10B9 "\x2D19;")
    (make-fold-case #\x10BA "\x2D1A;")
    (make-fold-case #\x10BB "\x2D1B;")
    (make-fold-case #\x10BC "\x2D1C;")
    (make-fold-case #\x10BD "\x2D1D;")
    (make-fold-case #\x10BE "\x2D1E;")
    (make-fold-case #\x10BF "\x2D1F;")
    (make-fold-case #\x10C0 "\x2D20;")
    (make-fold-case #\x10C1 "\x2D21;")
    (make-fold-case #\x10C2 "\x2D22;")
    (make-fold-case #\x10C3 "\x2D23;")
    (make-fold-case #\x10C4 "\x2D24;")
    (make-fold-case #\x10C5 "\x2D25;")
    (make-fold-case #\x1E96 "\x0068;\x0331;")
    (make-fold-case #\x1E97 "\x0074;\x0308;")
    (make-fold-case #\x1E98 "\x0077;\x030A;")
    (make-fold-case #\x1E99 "\x0079;\x030A;")
    (make-fold-case #\x1E9A "\x0061;\x02BE;")
    (make-fold-case #\x1E9B "\x1E61;")
    (make-fold-case #\x1E9E "\x0073;\x0073;")
    (make-fold-case #\x1EFA "\x1EFB;")
    (make-fold-case #\x1EFC "\x1EFD;")
    (make-fold-case #\x1EFE "\x1EFF;")
    (make-fold-case #\x1F50 "\x03C5;\x0313;")
    (make-fold-case #\x1F52 "\x03C5;\x0313;\x0300;")
    (make-fold-case #\x1F54 "\x03C5;\x0313;\x0301;")
    (make-fold-case #\x1F56 "\x03C5;\x0313;\x0342;")
    (make-fold-case #\x1F80 "\x1F00;\x03B9;")
    (make-fold-case #\x1F81 "\x1F01;\x03B9;")
    (make-fold-case #\x1F82 "\x1F02;\x03B9;")
    (make-fold-case #\x1F83 "\x1F03;\x03B9;")
    (make-fold-case #\x1F84 "\x1F04;\x03B9;")
    (make-fold-case #\x1F85 "\x1F05;\x03B9;")
    (make-fold-case #\x1F86 "\x1F06;\x03B9;")
    (make-fold-case #\x1F87 "\x1F07;\x03B9;")
    (make-fold-case #\x1F88 "\x1F00;\x03B9;")
    (make-fold-case #\x1F89 "\x1F01;\x03B9;")
    (make-fold-case #\x1F8A "\x1F02;\x03B9;")
    (make-fold-case #\x1F8B "\x1F03;\x03B9;")
    (make-fold-case #\x1F8C "\x1F04;\x03B9;")
    (make-fold-case #\x1F8D "\x1F05;\x03B9;")
    (make-fold-case #\x1F8E "\x1F06;\x03B9;")
    (make-fold-case #\x1F8F "\x1F07;\x03B9;")
    (make-fold-case #\x1F90 "\x1F20;\x03B9;")
    (make-fold-case #\x1F91 "\x1F21;\x03B9;")
    (make-fold-case #\x1F92 "\x1F22;\x03B9;")
    (make-fold-case #\x1F93 "\x1F23;\x03B9;")
    (make-fold-case #\x1F94 "\x1F24;\x03B9;")
    (make-fold-case #\x1F95 "\x1F25;\x03B9;")
    (make-fold-case #\x1F96 "\x1F26;\x03B9;")
    (make-fold-case #\x1F97 "\x1F27;\x03B9;")
    (make-fold-case #\x1F98 "\x1F20;\x03B9;")
    (make-fold-case #\x1F99 "\x1F21;\x03B9;")
    (make-fold-case #\x1F9A "\x1F22;\x03B9;")
    (make-fold-case #\x1F9B "\x1F23;\x03B9;")
    (make-fold-case #\x1F9C "\x1F24;\x03B9;")
    (make-fold-case #\x1F9D "\x1F25;\x03B9;")
    (make-fold-case #\x1F9E "\x1F26;\x03B9;")
    (make-fold-case #\x1F9F "\x1F27;\x03B9;")
    (make-fold-case #\x1FA0 "\x1F60;\x03B9;")
    (make-fold-case #\x1FA1 "\x1F61;\x03B9;")
    (make-fold-case #\x1FA2 "\x1F62;\x03B9;")
    (make-fold-case #\x1FA3 "\x1F63;\x03B9;")
    (make-fold-case #\x1FA4 "\x1F64;\x03B9;")
    (make-fold-case #\x1FA5 "\x1F65;\x03B9;")
    (make-fold-case #\x1FA6 "\x1F66;\x03B9;")
    (make-fold-case #\x1FA7 "\x1F67;\x03B9;")
    (make-fold-case #\x1FA8 "\x1F60;\x03B9;")
    (make-fold-case #\x1FA9 "\x1F61;\x03B9;")
    (make-fold-case #\x1FAA "\x1F62;\x03B9;")
    (make-fold-case #\x1FAB "\x1F63;\x03B9;")
    (make-fold-case #\x1FAC "\x1F64;\x03B9;")
    (make-fold-case #\x1FAD "\x1F65;\x03B9;")
    (make-fold-case #\x1FAE "\x1F66;\x03B9;")
    (make-fold-case #\x1FAF "\x1F67;\x03B9;")
    (make-fold-case #\x1FB2 "\x1F70;\x03B9;")
    (make-fold-case #\x1FB3 "\x03B1;\x03B9;")
    (make-fold-case #\x1FB4 "\x03AC;\x03B9;")
    (make-fold-case #\x1FB6 "\x03B1;\x0342;")
    (make-fold-case #\x1FB7 "\x03B1;\x0342;\x03B9;")
    (make-fold-case #\x1FBC "\x03B1;\x03B9;")
    (make-fold-case #\x1FBE "\x03B9;")
    (make-fold-case #\x1FC2 "\x1F74;\x03B9;")
    (make-fold-case #\x1FC3 "\x03B7;\x03B9;")
    (make-fold-case #\x1FC4 "\x03AE;\x03B9;")
    (make-fold-case #\x1FC6 "\x03B7;\x0342;")
    (make-fold-case #\x1FC7 "\x03B7;\x0342;\x03B9;")
    (make-fold-case #\x1FCC "\x03B7;\x03B9;")
    (make-fold-case #\x1FD2 "\x03B9;\x0308;\x0300;")
    (make-fold-case #\x1FD3 "\x03B9;\x0308;\x0301;")
    (make-fold-case #\x1FD6 "\x03B9;\x0342;")
    (make-fold-case #\x1FD7 "\x03B9;\x0308;\x0342;")
    (make-fold-case #\x1FE2 "\x03C5;\x0308;\x0300;")
    (make-fold-case #\x1FE3 "\x03C5;\x0308;\x0301;")
    (make-fold-case #\x1FE4 "\x03C1;\x0313;")
    (make-fold-case #\x1FE6 "\x03C5;\x0342;")
    (make-fold-case #\x1FE7 "\x03C5;\x0308;\x0342;")
    (make-fold-case #\x1FF2 "\x1F7C;\x03B9;")
    (make-fold-case #\x1FF3 "\x03C9;\x03B9;")
    (make-fold-case #\x1FF4 "\x03CE;\x03B9;")
    (make-fold-case #\x1FF6 "\x03C9;\x0342;")
    (make-fold-case #\x1FF7 "\x03C9;\x0342;\x03B9;")
    (make-fold-case #\x1FFC "\x03C9;\x03B9;")
    (make-fold-case #\x2126 "\x03C9;")
    (make-fold-case #\x212A "\x006B;")
    (make-fold-case #\x212B "\x00E5;")
    (make-fold-case #\x2132 "\x214E;")
    (make-fold-case #\x2183 "\x2184;")
    (make-fold-case #\x2C00 "\x2C30;")
    (make-fold-case #\x2C01 "\x2C31;")
    (make-fold-case #\x2C02 "\x2C32;")
    (make-fold-case #\x2C03 "\x2C33;")
    (make-fold-case #\x2C04 "\x2C34;")
    (make-fold-case #\x2C05 "\x2C35;")
    (make-fold-case #\x2C06 "\x2C36;")
    (make-fold-case #\x2C07 "\x2C37;")
    (make-fold-case #\x2C08 "\x2C38;")
    (make-fold-case #\x2C09 "\x2C39;")
    (make-fold-case #\x2C0A "\x2C3A;")
    (make-fold-case #\x2C0B "\x2C3B;")
    (make-fold-case #\x2C0C "\x2C3C;")
    (make-fold-case #\x2C0D "\x2C3D;")
    (make-fold-case #\x2C0E "\x2C3E;")
    (make-fold-case #\x2C0F "\x2C3F;")
    (make-fold-case #\x2C10 "\x2C40;")
    (make-fold-case #\x2C11 "\x2C41;")
    (make-fold-case #\x2C12 "\x2C42;")
    (make-fold-case #\x2C13 "\x2C43;")
    (make-fold-case #\x2C14 "\x2C44;")
    (make-fold-case #\x2C15 "\x2C45;")
    (make-fold-case #\x2C16 "\x2C46;")
    (make-fold-case #\x2C17 "\x2C47;")
    (make-fold-case #\x2C18 "\x2C48;")
    (make-fold-case #\x2C19 "\x2C49;")
    (make-fold-case #\x2C1A "\x2C4A;")
    (make-fold-case #\x2C1B "\x2C4B;")
    (make-fold-case #\x2C1C "\x2C4C;")
    (make-fold-case #\x2C1D "\x2C4D;")
    (make-fold-case #\x2C1E "\x2C4E;")
    (make-fold-case #\x2C1F "\x2C4F;")
    (make-fold-case #\x2C20 "\x2C50;")
    (make-fold-case #\x2C21 "\x2C51;")
    (make-fold-case #\x2C22 "\x2C52;")
    (make-fold-case #\x2C23 "\x2C53;")
    (make-fold-case #\x2C24 "\x2C54;")
    (make-fold-case #\x2C25 "\x2C55;")
    (make-fold-case #\x2C26 "\x2C56;")
    (make-fold-case #\x2C27 "\x2C57;")
    (make-fold-case #\x2C28 "\x2C58;")
    (make-fold-case #\x2C29 "\x2C59;")
    (make-fold-case #\x2C2A "\x2C5A;")
    (make-fold-case #\x2C2B "\x2C5B;")
    (make-fold-case #\x2C2C "\x2C5C;")
    (make-fold-case #\x2C2D "\x2C5D;")
    (make-fold-case #\x2C2E "\x2C5E;")
    (make-fold-case #\x2C60 "\x2C61;")
    (make-fold-case #\x2C62 "\x026B;")
    (make-fold-case #\x2C63 "\x1D7D;")
    (make-fold-case #\x2C64 "\x027D;")
    (make-fold-case #\x2C67 "\x2C68;")
    (make-fold-case #\x2C69 "\x2C6A;")
    (make-fold-case #\x2C6B "\x2C6C;")
    (make-fold-case #\x2C6D "\x0251;")
    (make-fold-case #\x2C6E "\x0271;")
    (make-fold-case #\x2C6F "\x0250;")
    (make-fold-case #\x2C70 "\x0252;")
    (make-fold-case #\x2C72 "\x2C73;")
    (make-fold-case #\x2C75 "\x2C76;")
    (make-fold-case #\x2C7E "\x023F;")
    (make-fold-case #\x2C7F "\x0240;")
    (make-fold-case #\x2C80 "\x2C81;")
    (make-fold-case #\x2C82 "\x2C83;")
    (make-fold-case #\x2C84 "\x2C85;")
    (make-fold-case #\x2C86 "\x2C87;")
    (make-fold-case #\x2C88 "\x2C89;")
    (make-fold-case #\x2C8A "\x2C8B;")
    (make-fold-case #\x2C8C "\x2C8D;")
    (make-fold-case #\x2C8E "\x2C8F;")
    (make-fold-case #\x2C90 "\x2C91;")
    (make-fold-case #\x2C92 "\x2C93;")
    (make-fold-case #\x2C94 "\x2C95;")
    (make-fold-case #\x2C96 "\x2C97;")
    (make-fold-case #\x2C98 "\x2C99;")
    (make-fold-case #\x2C9A "\x2C9B;")
    (make-fold-case #\x2C9C "\x2C9D;")
    (make-fold-case #\x2C9E "\x2C9F;")
    (make-fold-case #\x2CA0 "\x2CA1;")
    (make-fold-case #\x2CA2 "\x2CA3;")
    (make-fold-case #\x2CA4 "\x2CA5;")
    (make-fold-case #\x2CA6 "\x2CA7;")
    (make-fold-case #\x2CA8 "\x2CA9;")
    (make-fold-case #\x2CAA "\x2CAB;")
    (make-fold-case #\x2CAC "\x2CAD;")
    (make-fold-case #\x2CAE "\x2CAF;")
    (make-fold-case #\x2CB0 "\x2CB1;")
    (make-fold-case #\x2CB2 "\x2CB3;")
    (make-fold-case #\x2CB4 "\x2CB5;")
    (make-fold-case #\x2CB6 "\x2CB7;")
    (make-fold-case #\x2CB8 "\x2CB9;")
    (make-fold-case #\x2CBA "\x2CBB;")
    (make-fold-case #\x2CBC "\x2CBD;")
    (make-fold-case #\x2CBE "\x2CBF;")
    (make-fold-case #\x2CC0 "\x2CC1;")
    (make-fold-case #\x2CC2 "\x2CC3;")
    (make-fold-case #\x2CC4 "\x2CC5;")
    (make-fold-case #\x2CC6 "\x2CC7;")
    (make-fold-case #\x2CC8 "\x2CC9;")
    (make-fold-case #\x2CCA "\x2CCB;")
    (make-fold-case #\x2CCC "\x2CCD;")
    (make-fold-case #\x2CCE "\x2CCF;")
    (make-fold-case #\x2CD0 "\x2CD1;")
    (make-fold-case #\x2CD2 "\x2CD3;")
    (make-fold-case #\x2CD4 "\x2CD5;")
    (make-fold-case #\x2CD6 "\x2CD7;")
    (make-fold-case #\x2CD8 "\x2CD9;")
    (make-fold-case #\x2CDA "\x2CDB;")
    (make-fold-case #\x2CDC "\x2CDD;")
    (make-fold-case #\x2CDE "\x2CDF;")
    (make-fold-case #\x2CE0 "\x2CE1;")
    (make-fold-case #\x2CE2 "\x2CE3;")
    (make-fold-case #\x2CEB "\x2CEC;")
    (make-fold-case #\x2CED "\x2CEE;")
    (make-fold-case #\xA640 "\xA641;")
    (make-fold-case #\xA642 "\xA643;")
    (make-fold-case #\xA644 "\xA645;")
    (make-fold-case #\xA646 "\xA647;")
    (make-fold-case #\xA648 "\xA649;")
    (make-fold-case #\xA64A "\xA64B;")
    (make-fold-case #\xA64C "\xA64D;")
    (make-fold-case #\xA64E "\xA64F;")
    (make-fold-case #\xA650 "\xA651;")
    (make-fold-case #\xA652 "\xA653;")
    (make-fold-case #\xA654 "\xA655;")
    (make-fold-case #\xA656 "\xA657;")
    (make-fold-case #\xA658 "\xA659;")
    (make-fold-case #\xA65A "\xA65B;")
    (make-fold-case #\xA65C "\xA65D;")
    (make-fold-case #\xA65E "\xA65F;")
    (make-fold-case #\xA662 "\xA663;")
    (make-fold-case #\xA664 "\xA665;")
    (make-fold-case #\xA666 "\xA667;")
    (make-fold-case #\xA668 "\xA669;")
    (make-fold-case #\xA66A "\xA66B;")
    (make-fold-case #\xA66C "\xA66D;")
    (make-fold-case #\xA680 "\xA681;")
    (make-fold-case #\xA682 "\xA683;")
    (make-fold-case #\xA684 "\xA685;")
    (make-fold-case #\xA686 "\xA687;")
    (make-fold-case #\xA688 "\xA689;")
    (make-fold-case #\xA68A "\xA68B;")
    (make-fold-case #\xA68C "\xA68D;")
    (make-fold-case #\xA68E "\xA68F;")
    (make-fold-case #\xA690 "\xA691;")
    (make-fold-case #\xA692 "\xA693;")
    (make-fold-case #\xA694 "\xA695;")
    (make-fold-case #\xA696 "\xA697;")
    (make-fold-case #\xA722 "\xA723;")
    (make-fold-case #\xA724 "\xA725;")
    (make-fold-case #\xA726 "\xA727;")
    (make-fold-case #\xA728 "\xA729;")
    (make-fold-case #\xA72A "\xA72B;")
    (make-fold-case #\xA72C "\xA72D;")
    (make-fold-case #\xA72E "\xA72F;")
    (make-fold-case #\xA732 "\xA733;")
    (make-fold-case #\xA734 "\xA735;")
    (make-fold-case #\xA736 "\xA737;")
    (make-fold-case #\xA738 "\xA739;")
    (make-fold-case #\xA73A "\xA73B;")
    (make-fold-case #\xA73C "\xA73D;")
    (make-fold-case #\xA73E "\xA73F;")
    (make-fold-case #\xA740 "\xA741;")
    (make-fold-case #\xA742 "\xA743;")
    (make-fold-case #\xA744 "\xA745;")
    (make-fold-case #\xA746 "\xA747;")
    (make-fold-case #\xA748 "\xA749;")
    (make-fold-case #\xA74A "\xA74B;")
    (make-fold-case #\xA74C "\xA74D;")
    (make-fold-case #\xA74E "\xA74F;")
    (make-fold-case #\xA750 "\xA751;")
    (make-fold-case #\xA752 "\xA753;")
    (make-fold-case #\xA754 "\xA755;")
    (make-fold-case #\xA756 "\xA757;")
    (make-fold-case #\xA758 "\xA759;")
    (make-fold-case #\xA75A "\xA75B;")
    (make-fold-case #\xA75C "\xA75D;")
    (make-fold-case #\xA75E "\xA75F;")
    (make-fold-case #\xA760 "\xA761;")
    (make-fold-case #\xA762 "\xA763;")
    (make-fold-case #\xA764 "\xA765;")
    (make-fold-case #\xA766 "\xA767;")
    (make-fold-case #\xA768 "\xA769;")
    (make-fold-case #\xA76A "\xA76B;")
    (make-fold-case #\xA76C "\xA76D;")
    (make-fold-case #\xA76E "\xA76F;")
    (make-fold-case #\xA779 "\xA77A;")
    (make-fold-case #\xA77B "\xA77C;")
    (make-fold-case #\xA77D "\x1D79;")
    (make-fold-case #\xA77E "\xA77F;")
    (make-fold-case #\xA780 "\xA781;")
    (make-fold-case #\xA782 "\xA783;")
    (make-fold-case #\xA784 "\xA785;")
    (make-fold-case #\xA786 "\xA787;")
    (make-fold-case #\xA78B "\xA78C;")
    (make-fold-case #\xFB00 "\x0066;\x0066;")
    (make-fold-case #\xFB01 "\x0066;\x0069;")
    (make-fold-case #\xFB02 "\x0066;\x006C;")
    (make-fold-case #\xFB03 "\x0066;\x0066;\x0069;")
    (make-fold-case #\xFB04 "\x0066;\x0066;\x006C;")
    (make-fold-case #\xFB05 "\x0073;\x0074;")
    (make-fold-case #\xFB06 "\x0073;\x0074;")
    (make-fold-case #\xFB13 "\x0574;\x0576;")
    (make-fold-case #\xFB14 "\x0574;\x0565;")
    (make-fold-case #\xFB15 "\x0574;\x056B;")
    (make-fold-case #\xFB16 "\x057E;\x0576;")
    (make-fold-case #\xFB17 "\x0574;\x056D;")
    #|
    (make-fold-case #\x10400 "\x10428;")
    (make-fold-case #\x10401 "\x10429;")
    (make-fold-case #\x10402 "\x1042A;")
    (make-fold-case #\x10403 "\x1042B;")
    (make-fold-case #\x10404 "\x1042C;")
    (make-fold-case #\x10405 "\x1042D;")
    (make-fold-case #\x10406 "\x1042E;")
    (make-fold-case #\x10407 "\x1042F;")
    (make-fold-case #\x10408 "\x10430;")
    (make-fold-case #\x10409 "\x10431;")
    (make-fold-case #\x1040A "\x10432;")
    (make-fold-case #\x1040B "\x10433;")
    (make-fold-case #\x1040C "\x10434;")
    (make-fold-case #\x1040D "\x10435;")
    (make-fold-case #\x1040E "\x10436;")
    (make-fold-case #\x1040F "\x10437;")
    (make-fold-case #\x10410 "\x10438;")
    (make-fold-case #\x10411 "\x10439;")
    (make-fold-case #\x10412 "\x1043A;")
    (make-fold-case #\x10413 "\x1043B;")
    (make-fold-case #\x10414 "\x1043C;")
    (make-fold-case #\x10415 "\x1043D;")
    (make-fold-case #\x10416 "\x1043E;")
    (make-fold-case #\x10417 "\x1043F;")
    (make-fold-case #\x10418 "\x10440;")
    (make-fold-case #\x10419 "\x10441;")
    (make-fold-case #\x1041A "\x10442;")
    (make-fold-case #\x1041B "\x10443;")
    (make-fold-case #\x1041C "\x10444;")
    (make-fold-case #\x1041D "\x10445;")
    (make-fold-case #\x1041E "\x10446;")
    (make-fold-case #\x1041F "\x10447;")
    (make-fold-case #\x10420 "\x10448;")
    (make-fold-case #\x10421 "\x10449;")
    (make-fold-case #\x10422 "\x1044A;")
    (make-fold-case #\x10423 "\x1044B;")
    (make-fold-case #\x10424 "\x1044C;")
    (make-fold-case #\x10425 "\x1044D;")
    (make-fold-case #\x10426 "\x1044E;")
    (make-fold-case #\x10427 "\x1044F;")
    |#
    )

  (initialize-case-maps)
  (initialize-case-folding)

)
