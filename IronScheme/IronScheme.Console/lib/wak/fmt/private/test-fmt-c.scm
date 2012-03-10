
(cond-expand
 (chicken (use test) (load "fmt-c.scm") ;;(use test fmt-c)
          )
 (gauche
  (use gauche.test)
  (use text.fmt)
  (use text.fmt.c)
  (define test-begin test-start)
  (define orig-test (with-module gauche.test test))
  (define-syntax test
    (syntax-rules ()
      ((test name expected expr)
       (orig-test name expected (lambda () expr)))
      ((test expected expr)
       (orig-test (let ((s (with-output-to-string (lambda () (write 'expr)))))
                    (substring s 0 (min 60 (string-length s))))
                  expected
                  (lambda () expr)))
      )))
 (else))

(test-begin "fmt-c")

(test "if (1) {
    2;
} else {
    3;
}
"
    (fmt #f (c-if 1 2 3)))

(test "if (x ? y : z) {
    2;
} else {
    3;
}
"
    (fmt #f (c-if (c-if 'x 'y 'z) 2 3)))

(test "int square (int x) {
    return x * x;
}
"
    (fmt #f (c-fun 'int 'square '((int x)) (c* 'x 'x))))

(test "int foo (int x, int y, int z) {
    if (x ? y : z) {
        return 2;
    } else {
        return 3;
    }
}
"
    (fmt #f (c-fun 'int 'foo '((int x) (int y) (int z))
                   (c-if (c-if 'x 'y 'z) 2 3))))

(test "void bar (int mode, const char *msg, unsigned int arg) {
    if (mode == 1) {
        printf(msg);
    } else {
        printf(msg, arg);
    }
}
"
    (fmt #f (c-fun 'void 'bar
                   '((int mode)
                     ((%pointer (const char)) msg)
                     ((unsigned int) arg))
                   (c-if (c== 'mode 1) '(printf msg) '(printf msg arg)))))

(test "while ((line = readline()) != EOF) {
    printf(\"%s\", line);
}
"
    (fmt #f (c-while (c!= (c= 'line '(readline)) 'EOF)
                     '(printf "%s" line))))

(test "switch (y) {
    case 1:
        x = 1;
        break;
    case 2:
        x = 4;
        break;
    default:
        x = 5;
        break;
}
"
    (fmt #f (c-switch 'y
                      (c-case 1 (c= 'x 1))
                      (c-case 2 (c= 'x 4))
                      (c-default (c= 'x 5)))))

(test "switch (y) {
    case 1:
        x = 1;
        break;
    case 2:
        x = 4;
    default:
        x = 5;
        break;
}
"
    (fmt #f (c-switch 'y
                      (c-case 1 (c= 'x 1))
                      (c-case/fallthrough 2 (c= 'x 4))
                      (c-default (c= 'x 5)))))

(test "switch (y) {
    case 1:
        x = 1;
        break;
    case 2:
        x = 4;
        break;
    default:
        x = 5;
        break;
}
"
    (fmt #f (c-switch 'y '((1) (= x 1)) '((2) (= x 4)) '(else (= x 5)))))

(test "switch (y) {
    case 1:
        x = 1;
        break;
    case 2:
        x = 4;
        break;
    default:
        x = 5;
        break;
}
"
    (fmt #f (c-expr '(switch y ((1) (= x 1)) ((2) (= x 4)) (else (= x 5))))))

(test "int q (int x) {
    switch (x) {
        case 1:
            return 1;
        case 2:
            return 4;
        default:
            return 5;
    }
}
"
    (fmt #f (c-fun 'int 'q '(x) (c-switch 'x '((1) 1) '((2) 4) '(else 5)))))

(test "for (i = 0; i < n; i++) {
    printf(\"i: %d\");
}
"
    (fmt #f (c-for (c= 'i 0) (c< 'i 'n) (c++/post 'i) '(printf "i: %d"))))

(test "a * x + b * y == c;\n"
    (fmt #f (c== (c+ (c* 'a 'x) (c* 'b 'y)) 'c)))
(test "a * x + b * y == c;\n"
    (fmt #f (c-expr '(== (+ (* a x) (* b y)) c))))

(test "(a + x) * (b + y) == c;\n"
    (fmt #f (c-expr '(== (* (+ a x) (+ b y)) c))))

(test
"(abracadabra!!!! + xylophone????)
  * (bananarama____ + yellowstonepark~~~~)
  * (cryptoanalysis + zebramania);\n"
    (fmt #f (c-expr '(* (+ abracadabra!!!! xylophone????)
                        (+ bananarama____ yellowstonepark~~~~)
                        (+ cryptoanalysis zebramania)))))

(test
"abracadabra(xylophone,
            bananarama,
            yellowstonepark,
            cryptoanalysis,
            zebramania,
            delightful,
            wubbleflubbery);\n"
    (fmt #f (c-expr '(abracadabra xylophone
                                  bananarama
                                  yellowstonepark
                                  cryptoanalysis
                                  zebramania
                                  delightful
                                  wubbleflubbery))))

(test "#define foo(x, y) (((x) + (y)))\n"
    (fmt #f (cpp-define '(foo (int x) (int y)) (c+ 'x 'y))))

(test "#define min(x, y) (((x) < (y)) ? (x) : (y))\n"
    (fmt #f (cpp-define '(min x y) (c-if (c< 'x 'y) 'x 'y))))

(test
"#define foo(x, y) (abracadabra(((x) + (y)), \\
                               xylophone, \\
                               bananarama, \\
                               yellowstonepark, \\
                               cryptoanalysis, \\
                               zebramania, \\
                               delightful, \\
                               wubbleflubbery))\n"
    (fmt #f (cpp-define '(foo x y)
                        '(abracadabra (+ x y)
                                      xylophone
                                      bananarama
                                      yellowstonepark
                                      cryptoanalysis
                                      zebramania
                                      delightful
                                      wubbleflubbery))))

(test "#ifndef FOO_H
#define FOO_H

extern int foo ();

#endif /* ! FOO_H */
"
    (fmt #f (cpp-wrap-header
             'FOO_H
             (c-extern (c-prototype 'int 'foo '())))))

(test "/* this is a /\\* nested *\\/ comment */"
    (fmt #f (c-comment " this is a " (c-comment " nested ") " comment ")))

;; the initial leading space is annoying but hard to remove at the
;; moment - the important thing is we preserve indentation in the body
(test "switch (y) {
    case 1:
        x = 1;
        break;
    
#ifdef H_TWO
    case 2:
        x = 4;
        break;
#endif /* H_TWO */
    default:
        x = 5;
        break;
}
"
    (fmt #f (c-expr
             `(switch y
                      ((1) (= x 1))
                      ,(cpp-ifdef 'H_TWO (c-case '(2) '(= x 4)))
                      (else (= x 5))))))

(test "#define eprintf(...) (fprintf(stderr, __VA_ARGS__))\n"
    (fmt #f (c-expr '(%define (eprintf . args) (fprintf stderr args)))))

(test "struct point {
    int x;
    int y;
};
"
    (fmt #f (c-expr `(struct point (x y)))))

(test "struct employee {
    short age;
    char *name;
    struct {
        int year;
        int month;
        int day;
    } dob;
} __attribute__ ((packed));
"
    (fmt #f (c-expr `(struct employee
                             ((short age)
                              ((%pointer char) name)
                              ((struct (year month day)) dob))
                             (%attribute packed)
                             ))))

(test "class employee {
    short age;
    char *name;
    struct {
        int year;
        int month;
        int day;
    } dob;
} __attribute__ ((packed));
"
    (fmt #f (c-class 'employee
                      '((short age)
                        ((%pointer char) name)
                        ((struct (year month day)) dob))
                      (c-attribute 'packed)
                      )))

(test "union object {
    char tag;
    struct {
        char tag;
        char *data;
    } string;
    struct {
        char tag;
        void *car;
        void *cdr;
    } pair;
    struct {
        char tag;
        unsigned int length;
        void *data;
    } vector;
};
"
    (fmt #f (c-expr
             '(union object
                     ((char tag)
                      ((struct ((char tag) ((* char) data))) string)
                      ((struct ((char tag)
                                ((* void) car)
                                ((* void) cdr)))
                       pair)
                      ((struct ((char tag)
                                ((unsigned int) length)
                                ((* void) data)))
                       vector)
                      )))))

(test "enum type_tags {
    TYPE_CHAR = 1,
    TYPE_FIXNUM,
    TYPE_BOOLEAN,
    TYPE_NULL,
    TYPE_EOF,
    TYPE_STRING,
    TYPE_PAIR,
    TYPE_VECTOR
};
"
    (fmt #f (c-expr '(enum type_tags ((TYPE_CHAR 1) TYPE_FIXNUM TYPE_BOOLEAN TYPE_NULL TYPE_EOF TYPE_STRING TYPE_PAIR TYPE_VECTOR)))))

(test "#define OP_EVAL 0xFE\n" (fmt #f (radix 16 (cpp-define 'OP_EVAL 254))))

(test "unsigned long table[SIZE] = {1, 2, 3, 4};\n"
    (fmt #f (c-var '(%array (unsigned long) SIZE) 'table '#(1 2 3 4))))

(test "int *array_of_ptr[];\n"
    (fmt #f (c-var '(%array (* int)) 'array_of_ptr)))

(test "int (*ptr_to_array)[];\n"
    (fmt #f (c-var '(* (%array int)) 'ptr_to_array)))

(test "foo **table = {{1, \"foo\"}, {2, \"bar\"}, {3, \"baz\"}, {4, \"qux\"}};\n"
    (fmt #f (c-var '(* (* foo)) 'table
                   '#(#(1 "foo") #(2 "bar") #(3 "baz") #(4 "qux")))))

(test "sexp (*f)(sexp, sexp) = NULL;\n"
    (fmt #f (c-var '(%fun sexp (sexp sexp)) 'f 'NULL)))

(test "sexp (*)(sexp) (*f)(sexp, sexp) = NULL;\n"
    (fmt #f (c-var '(%fun (%fun sexp (sexp)) (sexp sexp)) 'f 'NULL)))

(test "typedef double (*f)(double *, double, int);\n"
    (fmt #f (c-typedef '(%fun double ((* double) double int)) 'f)))

(test-end)

