;;;; fmt-c.scm -- fmt module for emitting/pretty-printing C code
;;
;; Copyright (c) 2007 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional state information

(define (fmt-in-macro? st) (fmt-ref st 'in-macro?))
(define (fmt-expression? st) (fmt-ref st 'expression?))
(define (fmt-return? st) (fmt-ref st 'return?))
(define (fmt-default-type st) (fmt-ref st 'default-type 'int))
(define (fmt-newline-before-brace? st) (fmt-ref st 'newline-before-brace?))
(define (fmt-braceless-bodies? st) (fmt-ref st 'braceless-bodies?))
(define (fmt-non-spaced-ops? st) (fmt-ref st 'non-spaced-ops?))
(define (fmt-no-wrap? st) (fmt-ref st 'no-wrap?))
(define (fmt-indent-space st) (fmt-ref st 'indent-space))
(define (fmt-switch-indent-space st) (fmt-ref st 'switch-indent-space))
(define (fmt-op st) (fmt-ref st 'op 'stmt))
(define (fmt-gen st) (fmt-ref st 'gen))

(define (c-in-expr proc) (fmt-let 'expression? #t proc))
(define (c-in-stmt proc) (fmt-let 'expression? #f proc))
(define (c-in-test proc) (fmt-let 'in-cond? #t (c-in-expr proc)))
(define (c-with-op op proc) (fmt-let 'op op proc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; be smart about operator precedence

(define (c-op-precedence x)
  (if (string? x)
      (cond
        ((or (string=? x ".") (string=? x "->")) 10)
        ((or (string=? x "++") (string=? x "--")) 20)
        ((string=? x "|") 65)
        ((string=? x "||") 75)
        ((string=? x "|=") 85)
        ((or (string=? x "+=") (string=? x "-=")) 85)
        (else 95))
      (case x
        ;;((|::|) 5) ; C++
        ((paren bracket) 5)
        ((dot arrow post-decrement post-increment) 10)
        ((**) 15)                       ; Perl
        ((unary+ unary- ! ~ cast unary-* unary-& sizeof) 20) ; ++ --
        ((=~ !~) 25)                    ; Perl
        ((* / %) 30)
        ((+ -) 35)
        ((<< >>) 40)
        ((< > <= >=) 45)
        ((lt gt le ge) 45)              ; Perl
        ((== !=) 50)
        ((eq ne cmp) 50)                ; Perl
        ((&) 55)
        ((^) 60)
        ;;((|\||) 65)
        ((&&) 70)
        ;;((|\|\||) 75)
        ;;((.. ...) 77)                   ; Perl
        ((?) 80)
        ((= *= /= %= &= ^= <<= >>=) 85) ; |\|=| ;  += -=
        ((comma) 90)
        ((=>) 90)                       ; Perl
        ((not) 92)                      ; Perl
        ((and) 93)                      ; Perl
        ((or xor) 94)                   ; Perl
        (else 95))))

(define (c-op< x y) (< (c-op-precedence x) (c-op-precedence y)))

(define (c-paren x) (cat "(" x ")"))

(define (c-maybe-paren op x)
  (lambda (st)
    ((fmt-let 'op op
              (if (or (fmt-in-macro? st) (c-op< (fmt-op st) op))
                  (c-paren x)
                  x))
     st)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default literals writer

(define (c-control-operator? x)
  (memq x '(if while switch repeat do for fun begin)))

(define (c-literal? x)
  (or (number? x) (string? x) (char? x) (boolean? x)))

(define (char->c-char c)
  (if (< 32 (char->integer c) 127)
      (if (or (eqv? #\' c) (eqv? #\\ c))
          (string #\' #\\ c #\')
          (string #\' c #\'))
      (case (char->integer c)
        ((7) "'\\a'") ((8) "'\\b'") ((9) "'\\t'") ((10) "'\\n'")
        ((11) "'\\v'") ((12) "'\\f'") ((13) "'\\r'")
        (else
         (string-append "'\\x" (number->string (char->integer c) 16) "'")))))

(define (c-format-number x)
  (if (and (integer? x) (exact? x))
      (lambda (st)
        ((case (fmt-radix st)
           ((16) (cat "0x" (string-upcase (number->string x 16))))
           ((8) (cat "0" (number->string x 8)))
           (else (dsp (number->string x))))
         st))
      (dsp (number->string x))))

(define (c-simple-literal x)
  (c-wrap-stmt
   (cond ((char? x) (dsp (char->c-char x)))
         ((boolean? x) (dsp (if x "1" "0")))
         ((number? x) (c-format-number x))
         ((null? x) (dsp "NULL"))
         ((eof-object? x) (dsp "EOF"))
         (else (dsp (write-to-string x))))))

(define (c-literal x)
  (lambda (st)
    ((if (and (fmt-in-macro? st) (c-op< 'paren (fmt-op st))
              (not (c-literal? x)))
         (c-paren (c-simple-literal x))
         (c-simple-literal x))
     st)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default expression generator

(define (c-expr/sexp x)
  (if (procedure? x)
      x
      (lambda (st)
        (cond
         ((pair? x)
          (case (car x)
            ((if) ((apply c-if (cdr x)) st))
            ((for) ((apply c-for (cdr x)) st))
            ((while) ((apply c-while (cdr x)) st))
            ((switch) ((apply c-switch (cdr x)) st))
            ((case) ((apply c-case (cdr x)) st))
            ((case/fallthrough) ((apply c-case/fallthrough (cdr x)) st))
            ((default) ((apply c-default (cdr x)) st))
            ((break) (c-break st))
            ((continue) (c-continue st))
            ((return) ((apply c-return (cdr x)) st))
            ((goto) ((apply c-goto (cdr x)) st))
            ((typedef) ((apply c-typedef (cdr x)) st))
            ((struct union class) ((apply c-struct/aux x) st))
            ((enum) ((apply c-enum (cdr x)) st))
            ((inline auto restrict register volatile extern static)
             ((cat (car x) " " (apply-cat (cdr x))) st))
            ;; non C-keywords must have some character invalid in a C
            ;; identifier to avoid conflicts - by default we prefix %
            ((vector-ref)
             ((c-wrap-stmt
               (cat (c-expr (cadr x)) "[" (c-expr (caddr x)) "]"))
              st))
            ((vector-set!)
             ((c= (c-in-expr
                   (cat (c-expr (cadr x)) "[" (c-expr (caddr x)) "]"))
                  (c-expr (cadddr x)))
              st))
            ((extern/C) ((apply c-extern/C (cdr x)) st))
            ((%apply) ((apply c-apply (cdr x)) st))
            ((%define) ((apply cpp-define (cdr x)) st))
            ((%include) ((apply cpp-include (cdr x)) st))
            ((%fun) ((apply c-fun (cdr x)) st))
            ((%cond)
             (let lp ((ls (cdr x)) (res '()))
               (if (null? ls)
                   ((apply c-if (reverse res)) st)
                   (lp (cdr ls)
                       (cons (if (pair? (cddar ls))
                                 (apply c-begin (cdar ls))
                                 (cadar ls))
                             (cons (caar ls) res))))))
            ((%prototype) ((apply c-prototype (cdr x)) st))
            ((%var) ((apply c-var (cdr x)) st))
            ((%begin) ((apply c-begin (cdr x)) st))
            ((%attribute) ((apply c-attribute (cdr x)) st))
            ((%line) ((apply cpp-line (cdr x)) st))
            ((%pragma %error %warning)
             ((apply cpp-generic (substring/shared (symbol->string (car x)) 1)
                     (cdr x)) st))
            ((%if %ifdef %ifndef %elif)
             ((apply cpp-if/aux (substring/shared (symbol->string (car x)) 1)
                     (cdr x)) st))
            ((%endif) ((apply cpp-endif (cdr x)) st))
            ((%block) ((apply c-braced-block (cdr x)) st))
            ((%comment) ((apply c-comment (cdr x)) st))
            ((:) ((apply c-label (cdr x)) st))
            ((%cast) ((apply c-cast (cdr x)) st))
            ((+ - & * / % ! ~ ^ && < > <= >= == != << >>
                = *= /= %= &= ^= >>= <<=) ; |\|| |\|\|| |\|=|
             ((apply c-op x) st))
            ((bitwise-and bit-and) ((apply c-op '& (cdr x)) st))
            ((bitwise-ior bit-or) ((apply c-op "|" (cdr x)) st))
            ((bitwise-xor bit-xor) ((apply c-op '^ (cdr x)) st))
            ((bitwise-not bit-not) ((apply c-op '~ (cdr x)) st))
            ((arithmetic-shift) ((apply c-op '<< (cdr x)) st))
            ((bitwise-ior= bit-or=) ((apply c-op "|=" (cdr x)) st))
            ((%or) ((apply c-op "||" (cdr x)) st))
            ((%. %field) ((apply c-op "." (cdr x)) st))
            ((%->) ((apply c-op "->" (cdr x)) st))
            (else
             (cond
              ((eq? (car x) (string->symbol "."))
               ((apply c-op "." (cdr x)) st))
              ((eq? (car x) (string->symbol "->"))
               ((apply c-op "->" (cdr x)) st))
              ((eq? (car x) (string->symbol "++"))
               ((apply c-op "++" (cdr x)) st))
              ((eq? (car x) (string->symbol "--"))
               ((apply c-op "--" (cdr x)) st))
              ((eq? (car x) (string->symbol "+="))
               ((apply c-op "+=" (cdr x)) st))
              ((eq? (car x) (string->symbol "-="))
               ((apply c-op "-=" (cdr x)) st))
              (else ((c-apply x) st))))))
         ((vector? x)
          ((c-wrap-stmt
            (fmt-try-fit
             (fmt-let 'no-wrap? #t
                      (cat "{" (fmt-join c-expr (vector->list x) ", ") "}"))
             (lambda (st)
               (let* ((col (fmt-col st))
                      (sep (string-append "," (make-nl-space col))))
                 ((cat "{" (fmt-join c-expr (vector->list x) sep)
                       "}" nl)
                  st)))))
           st))
         (else
          ((c-literal x) st))))))

(define (c-apply ls)
  (c-wrap-stmt
   (c-with-op
    'paren
    (cat (c-expr (car ls))
         (let ((flat (fmt-let 'no-wrap? #t (fmt-join c-expr (cdr ls) ", "))))
           (fmt-if
            fmt-no-wrap?
            (c-paren flat)
            (c-paren
             (fmt-try-fit
              flat
              (lambda (st)
                (let* ((col (fmt-col st))
                       (sep (string-append "," (make-nl-space col))))
                  ((fmt-join c-expr (cdr ls) sep) st)))))))))))

(define (c-expr x)
  (lambda (st) (((or (fmt-gen st) c-expr/sexp) x) st)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comments, with Emacs-friendly escaping of nested comments

(define (make-comment-writer st)
  (let ((output (fmt-ref st 'writer)))
    (lambda (str st)
      (let ((lim (- (string-length str) 1)))
        (let lp ((i 0) (st st))
          (let ((j (string-index str #\/ i)))
            (if j
                (let ((st (if (and (> j 0)
                                   (eqv? #\* (string-ref str (- j 1))))
                              (output
                               "\\/"
                               (output (substring/shared str i j) st))
                              (output (substring/shared str i (+ j 1)) st))))
                  (lp (+ j 1)
                      (if (and (< j lim) (eqv? #\* (string-ref str (+ j 1))))
                          (output "\\" st)
                          st)))
                (output (substring/shared str i) st))))))))

(define (c-comment . args)
  (lambda (st)
    ((cat "/*" (fmt-let 'writer (make-comment-writer st)
                        (apply-cat args))
          "*/")
     st)))

(define (make-block-comment-writer st)
  (let ((output (make-comment-writer st))
        (indent (string-append (make-nl-space (+ (fmt-col st) 1)) "* ")))
    (lambda (str st)
      (let ((lim (string-length str)))
        (let lp ((i 0) (st st))
          (let ((j (string-index str #\newline i)))
            (if j
                (lp (+ j 1)
                    (output indent (output (substring/shared str i j) st)))
                (output (substring/shared str i) st))))))))

(define (c-block-comment . args)
  (lambda (st)
    (let ((col (fmt-col st))
          (row (fmt-row st))
          (indent (c-current-indent-string st)))
      ((cat "/* "
            (fmt-let 'writer (make-block-comment-writer st) (apply-cat args))
            (lambda (st)
              (cond
                ((= row (fmt-row st)) ((dsp " */") st))
                ;;((= (+ 3 col) (fmt-col st)) ((dsp "*/") st))
                (else ((cat fl indent " */") st)))))
       st))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; preprocessor

(define (make-cpp-writer st)
  (let ((output (fmt-ref st 'writer)))
    (lambda (str st)
      (let lp ((i 0) (st st))
        (let ((j (string-index str #\newline i)))
          (if j
              (lp (+ j 1)
                  (output
                   nl-str
                   (output " \\" (output (substring/shared str i j) st))))
              (output (substring/shared str i) st)))))))

(define (cpp-include file)
  (if (string? file)
      (cat fl "#include " (wrt file) fl)
      (cat fl "#include <" file ">" fl)))

(define (list-dot x)
  (cond ((pair? x) (list-dot (cdr x)))
        ((null? x) #f)
        (else x)))

(define (replace-tree from to x)
  (let replace ((x x))
    (cond ((eq? x from) to)
          ((pair? x) (cons (replace (car x)) (replace (cdr x))))
          (else x))))

(define (cpp-define x . body)
  (define (name-of x) (c-expr (if (pair? x) (cadr x) x)))
  (lambda (st)
    (let* ((body (cond
                   ((and (pair? x) (list-dot x))
                    => (lambda (dot)
                         (if (eq? dot '...)
                             body
                             (replace-tree dot '__VA_ARGS__ body))))
                   (else body)))
           (tail
            (if (pair? body)
                (cat " "
                     (fmt-let 'writer (make-cpp-writer st)
                              (fmt-let 'in-macro? (pair? x)
                                       ((if (or (not (pair? x))
                                                (and (null? (cdr body))
                                                     (c-literal? (car body))))
                                            (lambda (x) x)
                                            c-paren)
                                        (c-in-expr (apply c-begin body))))))
                (lambda (x) x))))
      ((c-in-expr
        (if (pair? x)
            (cat fl "#define " (name-of (car x))
                 (c-paren
                  (fmt-join/dot name-of
                                (lambda (dot) (dsp "..."))
                                (cdr x)
                                ", "))
                 tail fl)
            (cat fl "#define " (c-expr x) tail fl)))
       st))))

(define (cpp-expr x)
  (if (or (symbol? x) (string? x)) (dsp x) (c-expr x)))

(define (cpp-if/aux name check . o)
  (let ((pass (and (pair? o) (car o)))
        (fail (and (pair? o) (pair? (cdr o)) (cadr o))))
    (lambda (st)
      (let ((indent (c-current-indent-string st)))
        ((cat fl "#" name " " (cpp-expr check) fl
              (if pass (cat indent pass) "")
              (if fail (cat fl "#else" fl indent fail) "")
              (if (or pass fail)
                  (cat fl
                       "#endif"
                       (if (member name '("ifdef" "ifndef"))
                           (cat " "
                                (c-comment
                                 " " (if (equal? name "ifndef") "! " "")
                                 check " "))
                           "")
                       fl)
                  ""))
         st)))))

(define (cpp-if check . o)
  (apply cpp-if/aux "if" check o))
(define (cpp-ifdef check . o)
  (apply cpp-if/aux "ifdef" check o))
(define (cpp-ifndef check . o)
  (apply cpp-if/aux "ifndef" check o))
(define (cpp-elif check . o)
  (apply cpp-if/aux "elif" check o))
(define (cpp-endif . o)
  (cat fl "#endif " (if (pair? o) (c-comment (car o)) "") fl))

(define (cpp-wrap-header name . body)
  (let ((name name)) ; consider auto-mangling
    (cpp-ifndef name (c-begin (cpp-define name) nl (apply c-begin body) nl))))

(define (cpp-line num . o)
  (cat fl "#line " num (if (pair? o) (cat " " (car o)) "") fl))

(define (cpp-generic name . ls)
  (cat fl "#" name (apply-cat ls) fl))

(define (cpp-undef . args) (apply cpp-generic "undef" args))
(define (cpp-pragma . args) (apply cpp-generic "pragma" args))
(define (cpp-error . args) (apply cpp-generic "error" args))
(define (cpp-warning . args) (apply cpp-generic "warning" args))

(define (cpp-stringify x)
  (cat "#" x))

(define (cpp-sym-cat . args)
  (fmt-join dsp args " ## "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general indentation and brace rules

(define (c-current-indent-string st . o)
  (make-space (max 0 (+ (fmt-col st) (if (pair? o) (car o) 0)))))

(define (c-indent st . o)
  (dsp (make-space (max 0 (+ (fmt-col st) (or (fmt-indent-space st) 4)
                             (if (pair? o) (car o) 0))))))

(define (c-indent/switch st)
  (dsp (make-space (+ (fmt-col st) (or (fmt-switch-indent-space st) 4)))))

(define (c-open-brace st)
  (if (fmt-newline-before-brace? st)
      (cat nl (c-current-indent-string st) "{" nl)
      (cat " {" nl)))

(define (c-close-brace st)
  (dsp "}"))

(define (c-wrap-stmt x)
  (fmt-if fmt-expression?
          (c-expr x)
          (cat (fmt-if fmt-return? "return " "")
               (c-in-expr (c-expr x)) ";" nl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code blocks

(define (c-block . args)
  (apply c-block/aux 0 args))

(define (c-block/aux offset header body0 . body)
   (let ((inner (apply c-begin body0 body)))
     (if (or (pair? body)
             (not (or (c-literal? body0)
                      (and (pair? body0)
                           (not (c-control-operator? (car body0)))))))
         (c-braced-block/aux offset header inner)
         (lambda (st)
           (if (fmt-braceless-bodies? st)
               ((cat header fl (c-indent st offset) inner fl) st)
               ((c-braced-block/aux offset header inner) st))))))

(define (c-braced-block . args)
  (apply c-braced-block/aux 0 args))

(define (c-braced-block/aux offset header . body)
   (lambda (st)
     ((cat header (c-open-brace st) (c-indent st offset)
           (apply c-begin body) fl
           (c-current-indent-string st offset) (c-close-brace st))
      st)))

(define (c-begin . args)
  (apply c-begin/aux #f args))

(define (c-begin/aux ret? body0 . body)
   (if (null? body)
       (c-expr body0)
       (lambda (st)
         (if (fmt-expression? st)
             ((fmt-try-fit
               (fmt-let 'no-wrap? #t (fmt-join c-expr (cons body0 body) ", "))
               (lambda (st)
                 (let ((indent (c-current-indent-string st)))
                   ((fmt-join c-expr (cons body0 body) (cat "," nl indent)) st))))
              st)
             (let ((orig-ret? (fmt-return? st)))
               ((fmt-join/last c-expr
                               (lambda (x) (fmt-let 'return? orig-ret? (c-expr x)))
                               (cons body0 body)
                               (cat fl (c-current-indent-string st)))
                (fmt-set! st 'return? (and ret? orig-ret?))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structures

(define (c-struct/aux type x . o)
  (let* ((name (if (null? o) (if (or (symbol? x) (string? x)) x #f) x))
         (body (if name (car o) x))
         (o (if (null? o) o (cdr o))))
    (c-wrap-stmt
     (cat
      (c-braced-block
       (cat type (if (and name (not (equal? name ""))) (cat " " name) ""))
       (cat
        (c-in-stmt
         (if (list? body)
             (apply c-begin (map c-wrap-stmt (map c-param body)))
             (c-wrap-stmt (c-expr body))))))
      (if (pair? o) (cat " " (apply c-begin o)) (dsp ""))))))

(define (c-struct . args) (apply c-struct/aux "struct" args))
(define (c-union . args) (apply c-struct/aux "union" args))
(define (c-class . args) (apply c-struct/aux "class" args))

(define (c-enum x . o)
  (define (c-enum-one x)
    (if (pair? x) (cat (car x) " = " (c-expr (cadr x))) (dsp x)))
  (let* ((name (if (null? o) (if (or (symbol? x) (string? x)) x #f) x))
         (vals (if name (car o) x)))
    (c-wrap-stmt
     (cat
      (c-braced-block
       (if name (cat "enum " name) (dsp "enum"))
       (c-in-expr (apply c-begin (map c-enum-one vals))))))))

(define (c-attribute . args)
  (cat "__attribute__ ((" (fmt-join c-expr args ", ") "))"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic control structures

(define (c-while check . body)
  (cat (c-block (cat "while (" (c-in-test check) ")")
                (c-in-stmt (apply c-begin body)))
       fl))

(define (c-for init check update . body)
  (cat
   (c-block
    (c-in-expr
     (cat "for (" (c-expr init) "; " (c-in-test check) "; "
          (c-expr update ) ")"))
    (c-in-stmt (apply c-begin body)))
   fl))

(define (c-param x)
  (cond
    ((procedure? x) x)
    ((pair? x) (c-type (car x) (cadr x)))
    (else (cat (lambda (st) ((c-type (fmt-default-type st)) st)) " " x))))

(define (c-param-list ls)
  (c-in-expr (fmt-join/dot c-param (lambda (dot) (dsp "...")) ls ", ")))

(define (c-fun type name params . body)
  (cat (c-block (c-in-expr (c-prototype type name params))
                (fmt-let 'return? (not (eq? 'void type))
                         (c-in-stmt (apply c-begin body))))
       fl))

(define (c-prototype type name params . o)
  (c-wrap-stmt
   (cat (c-type type) " " (c-expr name) " (" (c-param-list params) ")"
        (fmt-join/prefix c-expr o " "))))

(define (c-static x) (cat "static " (c-expr x)))
(define (c-const x) (cat "const " (c-expr x)))
(define (c-restrict x) (cat "restrict " (c-expr x)))
(define (c-volatile x) (cat "volatile " (c-expr x)))
(define (c-auto x) (cat "auto " (c-expr x)))
(define (c-inline x) (cat "inline " (c-expr x)))
(define (c-extern x) (cat "extern " (c-expr x)))
(define (c-extern/C . body)
  (cat "extern \"C\" {" nl (apply c-begin body) nl "}" nl))

(define (c-type type . o)
  (let ((name (and (pair? o) (car o))))
    (cond
     ((pair? type)
      (case (car type)
        ((%fun)
         (cat (c-type (cadr type) #f)
              " (*" (or name "") ")("
              (fmt-join (lambda (x) (c-type x #f)) (caddr type) ", ") ")"))
        ((%array)
         (let ((name (cat name "[" (if (pair? (cddr type))
                                       (c-expr (caddr type))
                                       "")
                          "]")))
           (c-type (cadr type) name)))
        ((%pointer *)
         (let ((name (cat "*" (if name (c-expr name) ""))))
           (c-type (cadr type)
                   (if (and (pair? (cadr type)) (eq? '%array (caadr type)))
                       (c-paren name)
                       name))))
        ((enum) (apply c-enum name (cdr type)))
        ((struct union class)
         (cat (apply c-struct/aux (car type) (cdr type)) " " name))
        (else (fmt-join/last c-expr (lambda (x) (c-type x name)) type " "))))
     ((not type)
      (lambda (st) ((c-type (or (fmt-default-type st) 'int) name) st)))
     (else
      (cat (if (eq? '%pointer type) '* type) (if name (cat " " name) ""))))))

(define (c-var type name . init)
  (c-wrap-stmt
   (if (pair? init)
       (cat (c-type type name) " = " (c-expr (car init)))
       (c-type type (if (pair? name)
                        (fmt-join c-expr name ", ")
                        (c-expr name))))))

(define (c-cast type expr)
  (cat "(" (c-type type) ")" (c-expr expr)))

(define (c-typedef type alias . o)
  (c-wrap-stmt
   (cat "typedef " (c-type type alias) (fmt-join/prefix c-expr o " "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalized IF: allows multiple tail forms for if/else if/.../else
;; blocks.  A final ELSE can be signified with a test of #t or 'else,
;; or by simply using an odd number of expressions (by which the
;; normal 2 or 3 clause IF forms are special cases).

(define (c-if/stmt c p . rest)
  (lambda (st)
    (let ((indent (c-current-indent-string st)))
      ((let lp ((c c) (p p) (ls rest))
         (if (or (eq? c 'else) (eq? c #t))
             (if (not (null? ls))
                 (error "forms after else clause in IF" c p ls)
                 (cat (c-block/aux -1 " else" p) fl))
             (let ((tail (if (pair? ls)
                             (if (pair? (cdr ls))
                                 (lp (car ls) (cadr ls) (cddr ls))
                                 (lp 'else (car ls) '()))
                             fl)))
               (cat (c-block/aux
                     (if (eq? ls rest) 0 -1)
                     (cat (if (eq? ls rest) (lambda (x) x) " else ")
                          "if (" (c-in-test (c-expr c)) ")") p)
                    tail))))
       st))))

(define (c-if/expr c p . rest)
  (let lp ((c c) (p p) (ls rest))
    (cond
      ((or (eq? c 'else) (eq? c #t))
       (if (not (null? ls))
           (error "forms after else clause in IF" c p ls)
           (c-expr p)))
      ((pair? ls)
       (cat (c-in-test (c-expr c)) " ? " (c-expr p) " : "
            (if (pair? (cdr ls))
                (lp (car ls) (cadr ls) (cddr ls))
                (lp 'else (car ls) '()))))
      (else
       (c-or (c-in-test (c-expr c)) (c-expr p))))))

(define (c-if . args)
  (fmt-if fmt-expression?
          (apply c-if/expr args)
          (apply c-if/stmt args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch statements, automatic break handling

(define (c-label name)
  (lambda (st)
    (let ((indent (make-space (max 0 (- (fmt-col st) 2)))))
      ((cat fl indent name ":" fl) st))))

(define c-break
  (c-wrap-stmt (dsp "break")))
(define c-continue
  (c-wrap-stmt (dsp "continue")))
(define (c-return . result)
  (if (pair? result)
      (c-wrap-stmt (cat "return " (c-expr (car result))))
      (c-wrap-stmt (dsp "return"))))
(define (c-goto label)
  (c-wrap-stmt (cat "goto " (c-expr label))))

(define (c-switch val . clauses)
  (lambda (st)
    ((cat "switch (" (c-in-expr val) ")" (c-open-brace st)
          (c-indent/switch st)
          (c-in-stmt (apply c-begin/aux #t (map c-switch-clause clauses))) fl
          (c-current-indent-string st) (c-close-brace st) fl)
     st)))

(define (c-switch-clause/breaks x)
  (lambda (st)
    (let* ((break? (car x))
           (indent (c-current-indent-string st))
           (indent-body (c-indent st))
           (sep (string-append ":" nl-str indent)))
      ((cat (c-in-expr
             (fmt-join/suffix
              dsp
              (if (pair? (cadr x))
                  (map (lambda (y) (cat (dsp "case ") (c-expr y)))
                       (cadr x))
                  (list (dsp "default")))
              sep))
            (make-space (or (fmt-indent-space st) 4))
            (fmt-join c-expr (cddr x) indent-body)
            (if (and break? (not (fmt-return? st)))
                (cat fl indent-body c-break)
                ""))
       st))))

(define (c-switch-clause x)
  (if (procedure? x) x (c-switch-clause/breaks (cons #t x))))
(define (c-switch-clause/no-break x)
  (if (procedure? x) x (c-switch-clause/breaks (cons #f x))))

(define (c-case x . body)
  (c-switch-clause (cons (if (pair? x) x (list x)) body)))
(define (c-case/fallthrough x . body)
  (c-switch-clause/no-break (cons (if (pair? x) x (list x)) body)))
(define (c-default . body)
  (c-switch-clause/breaks (cons #t (cons 'else body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operators

(define (c-op op first . rest)
  (if (null? rest)
      (c-unary-op op first)
      (apply c-binary-op op first rest)))

(define (c-binary-op op . ls)
  (define (lit-op? x) (or (c-literal? x) (symbol? x)))
  (let ((str (display-to-string op)))
    (c-wrap-stmt
     (c-maybe-paren
      op
      (if (or (equal? str ".") (equal? str "->"))
          (fmt-join c-expr ls str)
          (let ((flat
                 (fmt-let 'no-wrap? #t
                          (lambda (st)
                            ((fmt-join c-expr
                                       ls
                                       (if (and (fmt-non-spaced-ops? st)
                                                (every lit-op? ls))
                                           str
                                           (string-append " " str " ")))
                             st)))))
            (fmt-if
             fmt-no-wrap?
             flat
             (fmt-try-fit
              flat
              (lambda (st)
                   ((fmt-join c-expr
                              ls
                              (cat nl (make-space (+ 2 (fmt-col st))) str " "))
                    st))))))))))

(define (c-unary-op op x)
  (c-wrap-stmt
   (cat (display-to-string op) (c-maybe-paren op (c-expr x)))))

;; some convenience definitions

(define (c++ . args) (apply c-op "++" args))
(define (c-- . args) (apply c-op "--" args))
(define (c+ . args) (apply c-op '+ args))
(define (c- . args) (apply c-op '- args))
(define (c* . args) (apply c-op '* args))
(define (c/ . args) (apply c-op '/ args))
(define (c% . args) (apply c-op '% args))
(define (c& . args) (apply c-op '& args))
;; (define (|c\|| . args) (apply c-op '|\|| args))
(define (c^ . args) (apply c-op '^ args))
(define (c~ . args) (apply c-op '~ args))
(define (c! . args) (apply c-op '! args))
(define (c&& . args) (apply c-op '&& args))
;; (define (|c\|\|| . args) (apply c-op '|\|\|| args))
(define (c<< . args) (apply c-op '<< args))
(define (c>> . args) (apply c-op '>> args))
(define (c== . args) (apply c-op '== args))
(define (c!= . args) (apply c-op '!= args))
(define (c< . args) (apply c-op '< args))
(define (c> . args) (apply c-op '> args))
(define (c<= . args) (apply c-op '<= args))
(define (c>= . args) (apply c-op '>= args))
(define (c= . args) (apply c-op '= args))
(define (c+= . args) (apply c-op "+=" args))
(define (c-= . args) (apply c-op "-=" args))
(define (c*= . args) (apply c-op '*= args))
(define (c/= . args) (apply c-op '/= args))
(define (c%= . args) (apply c-op '%= args))
(define (c&= . args) (apply c-op '&= args))
;; (define (|c\|=| . args) (apply c-op '|\|=| args))
(define (c^= . args) (apply c-op '^= args))
(define (c<<= . args) (apply c-op '<<= args))
(define (c>>= . args) (apply c-op '>>= args))

(define (c. . args) (apply c-op "." args))
(define (c-> . args) (apply c-op "->" args))

(define (c-bit-or . args) (apply c-op "|" args))
(define (c-or . args) (apply c-op "||" args))
(define (c-bit-or= . args) (apply c-op "|=" args))

(define (c++/post x)
  (cat (c-maybe-paren 'post-increment (c-expr x)) "++"))
(define (c--/post x)
  (cat (c-maybe-paren 'post-decrement (c-expr x)) "--"))

