;;;; fmt-color.scm -- colored output
;;
;; Copyright (c) 2006-2007 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (fmt-color st) (fmt-ref st 'color))
(define (fmt-in-html? st) (fmt-ref st 'in-html?))
(define (fmt-use-html-font? st) (fmt-ref st 'use-html-font?))

(define (color->ansi x)
  (if (number? x)
      (let ((r (arithmetic-shift x -16))
            (g (bitwise-and (arithmetic-shift x -8) #xFF))
            (b (bitwise-and x #xFF)))
        ;; just picks the highest color value - need to detect blends
        (color->ansi
         (cond
           ((> r g) (if (> r b) 'red 'blue))
           ((> g b) 'green)
           (else 'blue))))
      (case x
        ((bold) "1")
        ((dark) "2")
        ((underline) "4")
        ((black) "30")
        ((red) "31")
        ((green) "32")
        ((yellow) "33")
        ((blue) "34")
        ((magenta) "35")
        ((cyan) "36")
        ((white) "37")
        (else "0"))))

(define (ansi-escape color)
  (cat (integer->char 27) "[" (color->ansi color) "m"))

(define (fmt-in-html . args)
  (fmt-let 'in-html? #t (apply-cat args)))

(define (fmt-colored color . args)
  (fmt-if fmt-in-html?
          (cond
            ((eq? color 'bold)
             (cat "<b>" (apply-cat args) "</b>"))
            ((eq? color 'underline)
             (cat "<u>" (apply-cat args) "</u>"))
            (else
             (let ((cname (if (number? color) (cat "#" color) color)))
               (fmt-if fmt-use-html-font?
                       (cat "<font color=\"" cname "\">" (apply-cat args)
                            "</font>")
                       (cat "<span style=color:\"" cname "\">"
                            (apply-cat args) "</span>")))))
          (lambda (st)
            (let ((old-color (fmt-color st)))
              ((fmt-let 'color color
                        (cat (ansi-escape color)
                             (apply-cat args)
                             (if (or (memv color '(bold underline))
                                     (memv old-color '(bold underline)))
                                 (ansi-escape 'reset)
                                 (lambda (st) st))
                             (ansi-escape old-color)))
               st)))))

(define (fmt-red . args) (fmt-colored 'red (apply-cat args)))
(define (fmt-blue . args) (fmt-colored 'blue (apply-cat args)))
(define (fmt-green . args) (fmt-colored 'green (apply-cat args)))
(define (fmt-cyan . args) (fmt-colored 'cyan (apply-cat args)))
(define (fmt-yellow . args) (fmt-colored 'yellow (apply-cat args)))
(define (fmt-magenta . args) (fmt-colored 'magenta (apply-cat args)))
(define (fmt-white . args) (fmt-colored 'white (apply-cat args)))
(define (fmt-black . args) (fmt-colored 'black (apply-cat args)))
(define (fmt-bold . args) (fmt-colored 'bold (apply-cat args)))
(define (fmt-underline . args) (fmt-colored 'underline (apply-cat args)))

