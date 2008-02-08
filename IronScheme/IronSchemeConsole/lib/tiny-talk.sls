#!r6rs
;;; FILE      "tiny-talk.sls"
;;; IMPLEMENTS Simple dynamic object system
;;;	- Lexically scoped objects
;;;	- Naming not required
;;;	- Selector [Smalltalk style] dispatch
;;;	- Self-like delegation semantics
;;;       (build prototype, class, whatever OO style you want)
;;;	- No global tables
;;;	- Minimal syntax, minimal mechanism [Really an OO toolkit]
;;;   	- R6RS Scheme
;;;
;;; AUTHOR    Ken [dot] Dickey [at] Whidbey [dot] Com

;;;COPYRIGHT (c) 2008 by Kenneth A Dickey. All rights reserved.
;;;
;;;Permission is hereby granted, free of charge, to any person
;;;obtaining a copy of this software and associated documentation
;;;files (the "Software"), to deal in the Software without
;;;restriction, including without limitation the rights to use,
;;;copy, modify, merge, publish, distribute, sublicense, and/or
;;;sell copies of the Software, and to permit persons to whom
;;;the Software is furnished to do so, subject to the following
;;;conditions:
;;;
;;;The above copyright notice and this permission notice shall
;;;be included in all copies or substantial portions of the Software.
;;;
;;;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;OTHER DEALINGS IN THE SOFTWARE.


;;;tiny-talk
(library (tiny-talk)
 (export ;; syntax
         $ object define-predicate
         ;; closures
         ->string object?
         custom-method-finder ;; for tiny-talk-plus
 )
 (import (rnrs)
         (rnrs records syntactic (6))
         (rnrs mutable-pairs (6))
         (rnrs lists (6))
 )
;;;

;; EXPORTED INTERFACE
;;
;; (object (<field-spec> ...) <method-spec> ...) -> an object instance
;; (object? thing) -- universal predicate
;; ($ <selector-sym> <obj> <arg> ...) -- send message to <obj>
;; (define-predicate <pred?>) -- defines a universal predicate
;; (->string thing) -- returns a string describing thing
;;
;; Messages understood by all tiny-talk object instances
;;  [$ field-names   <obj>] -- names of setter/getter field value access
;;  [$ shallow-clone <obj>] -- shallow-clone object [does NOT clone delegate(s)]
;;  [$ deep-clone    <obj>] -- deep-clone object    [DOES clone delegate(s)]
;;  [$ add-method!   <obj> <name-sym> <proc>]
;;  [$ remove-method! <obj> <name-sym>]
;;  [$ methods-alist <obj>] -- all (name . method) pairs
;;  [$ lookup   <obj>] -- symbol-> method or #f [single level (no delegate)]
;; Nice instances implement
;;  [$ ->string <obj>] -- descriptive string


;; SAMPLE USAGE
#|

;; Tiny-Talk is easy to use to create and explore interactively.
;; Once you know what you want, you can optimize.

> (import (tiny-talk))

;; make a point -- just an anonymous object.

> (define p1 (object ((x 1)(y 2))))

;; Look at the object with the default "printer"

> (->string p1)
"#[instance x: 1 y: 2]"

;; Do the same thing as a message

> [$ ->string p1]
Unhandled exception
 Condition components:
   1. &error
   2. &who: ->string
   3. &message: "not applicable to object: #[instance x: 1 y: 2]"
   4. &irritants: ((#[obj #<procedure find-method>]))

;; Well, looks like we should add an implementation of ->string !

> ($ add-method! p1 
     '->string 
     (lambda (self)
         (string-append "(point "
                         (->string [$ x self])
                         " "
                         (->string [$ y self])
                         ")")))
> [$ ->string p1]
"(point 1 2)"

;; Now, how do we know it is a point?

> (define-predicate point?)

> (point? 3)
#f    ;; Works for non-points

> (point? p1)
#f    ;; Oops!

> [$ add-method! p1 'point? (lambda (self) #t)]

;; All "method" lambdas take a "self" argument.

> (point? p1)
#t   ;; Ah, better.

;; Use Self style cloning to get a new point

> (define p2 [$ shallow-clone p1])

> [$ ->string p2]
"(point 1 2)"

;; Setters & getters use the same syntax.

> [$ x p2 23]
23

> [$ y p2 45]
45

> [$ ->string p2]
"(point 23 45)"

> [$ x p2]
23

;; By the way, I am just using square brackets by convention to
;; avoid confusion between message sends and procedure calls.

> ($ x p2)
23


;; OK for interactive use, but what if I know what I want?
;; Well, this is easy too.

>
(define (new-point x y)
  (unless (and (integer? x) (integer? y))
    (error 'new-point
           "use only integer values for points"
           x y))
  (object ([x x] [y y])
    [(point? self) #t]
    [(->string self)
     (string-append "(new-point "
                    (->string [$ x self])
                    " "
                    (->string [$ y self])
                    ")")
     ]
    [(distance-between self other)
     (unless (point? other)
       (error 'point:distance-between
              "Needs two points"
              self other))
     (let ( (dx (- [$ x self] [$ x other]))
            (dy (- [$ y self] [$ y other]))
            )
       (sqrt (+ (* dx dx) (* dy dy))))
     ]
    [(=? self other)
     (unless (point? other)
       (error 'point:=?
              "Don't know how compare point to non-point"
              self other))
     (and (= [$ x self] [$ x other]) (= [$ y self] [$ y other]))
     ]
    ;; other methods
    )
)

> (define p3 (new-point 100 200))

> [$ distance-between p3 p2]
173.07223925286226

;; note that p2 does NOT have a distance-between method

> [$ distance-between p2 p3]
Unhandled exception
 Condition components:
   1. &error
   2. &who: distance-between
   3. &message: "not applicable to object: (point 23 45)"
   4. &irritants: ((#[obj #<procedure find-method>] #[obj #<procedure find-method>]))

;; of course, we know how to fix that.  ;^)


;; Looking at methods, you might note that in methods we only
;; access objects by calling methods.

;; Let's use a delegate to model a class based OO system.

(define new-point
  (let ( [proto-point
          (object () ;; methods only
             [(point? self) #t]
             [(->string self)
              (string-append "(new-point "
                             (->string [$ x self])
                             " "
                             (->string [$ y self])
                             ")")
              ]
             [(distance-between self other)
              (unless (point? other)
                (error 'point:distance-between
                       "Needs two points"
                       self other))
              (let ( (dx (- [$ x self] [$ x other]))
                     (dy (- [$ y self] [$ y other]))
                     )
                (sqrt (+ (* dx dx) (* dy dy))))
              ]
             [(=? self other)
              (unless (point? other)
                (error 'point:=?
                       "Don't know how compare point to non-point"
                       self other))
              (and (= [$ x self] [$ x other]) (= [$ y self] [$ y other]))
              ]
             ;; other methods
             )
          ]
         )
    ;; each object gets its own "state" (x y)
    (lambda (x y)
      (unless (and (integer? x) (integer? y))
        (error 'new-point "use only integer values for points" x y))
      (object ([x x] [y y])
              ;; but delegates to shared code ["delegate" is special]
              [(delegate self) proto-point]))
) )

> (define p1 (new-point  21  30))
> (define p2 (new-point 300 400))

> (->string p1)
"(new-point 21 30)"

> [$ distance-between p1 p2]
463.401553730671
  
;; Now, we can add behavior to the "class" [prototype] of points

> [$ add-method! [$ delegate p1] 'color (lambda (self) 'green)]

;; now p1 and p2 share the new method

> [$ color p2]
green

> [$ color p1]
green

;; But adding a method to p1 does not add it to p2

> [$ add-method! p1 'name (lambda (self) 'p1)]

> [$ name p1]
p1

> [$ name p2]
Unhandled exception
 Condition components:
   1. &error
   2. &who: name
   3. &message: "not applicable to object: (new-point 300 400)"
   4. &irritants: ((#[obj #<procedure find-method>]))

;; OK, so what is this deep-clone thing?

;; deep-clone is for delegation based inheritance.
;; I.e. one makes delegates for objects one inherits from.
;;
;; In this case one does NOT want to share ancestors

> (define p3 [$ deep-clone p2])

> [$ x p3 45]
45

> [$ ->string p3]
"(new-point 45 400)"


> [$ add-method! [$ delegate p3] 'color (lambda (self) 'orange)]

> [$ color p3]
orange

> [$ color p2]
green

;; Yes, there is a diffence now.

> (define p4 [$ shallow-clone p3])

> [$ color p4]
orange

> [$ color [$ shallow-clone p1]]
green

;; "inheritance by delegation" means making an object
;; from which one inherits.

;; Here rectangle inherits from point

> (define (new-rectangle x y width height)
    (let ( (my-point (new-point x y)) )
      (object ( (width width) (height height) )
        [(rectangle? self) #t]
        [(->string self)
         (string-append "(new-rectangle x: "
                        (->string [$ x self])
                        " y: "
                        (->string [$ y self])
                        " width: "
                        (->string [$ width self])
                        " height: "
                        (->string [$ height self])
                        ")")
         ]
        [(delegate self) my-point])))

> (define r (new-rectangle 10 20 300 400))

> (->string r)

"(new-rectangle x: 10 y: 20 width: 300 height: 400)"

> [$ x r 20]
20

> (->string r)
"(new-rectangle x: 20 y: 20 width: 300 height: 400)"

;; Of course the point one inherits from is still there.

> (->string [$ delegate r])
"(new-point 20 20)"

;; To properly clone a rectangle, you need to use deep-clone.

> (define r2 [$ deep-clone r])

> [$ x r2 11]
11

> (->string r2)
"(new-rectangle x: 11 y: 20 width: 300 height: 400)"

> (->string r)
"(new-rectangle x: 20 y: 20 width: 300 height: 400)"

;; If shallow-clone had been used, the my-point instance
;; would have been shared and the behavior would have
;; been wrong.

;; One more sample: multiple inheritance.

> (define (every? pred? . rest)
    (for-all pred? rest))

> (define (new-color red green blue)
    (let ( (color-ok?
            (lambda (c) (and (integer? c) (<= 0 c 255))))
         )
      (unless (every? color-ok? red green blue)
        (error 'new-color 
               "colors must be integers between 0 and 255"
               red green blue))
      (object ([r red] [g green] [b blue])
         [(color? self) #t]
         [(->string self)
          (string-append "(new-color r: "
               (->string [$ r self]) " g: "
               (->string [$ g self]) " b: "
               (->string [$ b self]) ")")
          ])))

> (define (new-color-rect x y width height red green blue)
    (let* ( (color (new-color red green blue))
            (rect  (new-rectangle x y width height))
            (delegates (list color rect))
          )
      (object ()
         [(color-rect? self) #t]
         [(->string self)
          ;; boy, do we need SRFI-48: "format" !
          (string-append (->string color) (->string rect))
          ]
         [(delegate self) delegates])))

> (define cr (new-color-rect 1 2 20 30 0 200 0))

> [$ x cr 5]
5

> [$ g cr 100]
100

> (->string cr)
"(new-color r: 0 g: 100 b: 0)(new-rectangle x: 5 y: 2 width: 20 height: 30)"

> (define-predicate color?)

> (define-predicate rectangle?)

> (and (color? cr) (rectangle? cr) (point? cr))
#t   ;; Yes!!

;; Oh. Right.  You looked here expecting to see the code!  8^)
|#


;;; 

(define-record-type obj (fields dispatcher))


;;; [$ <selector-sym> <obj> <arg> ...]
(define-syntax $  ;; send [user syntax]
  (syntax-rules ()
    [($ <selector> <obj> <arg> ...)
     ;;=>
     ((-> '<selector> <obj>) <obj> <arg> ...)
    ]
) )


(define (-> sym obj) ;; send sym to obj to get a method
  (cond
   [(find-method sym obj)] ;; Answer method or #f
   [((custom-method-finder) sym obj)] ;; user addition
   [else (error-not-applicable sym obj)])
)

(define always-false (lambda (sym obj) #f))

(define custom-method-finder
  (let ( [sim+obj->method always-false] )
    (case-lambda
      [() sim+obj->method]
      [(proc)
       (unless (procedure? proc)
         (error 'custom-method-finder
                "requires a procedure (lambda (sym obj) ...)"
                proc))
        (set! sim+obj->method proc)
        proc]))
)


;;; (find-method obj selector) -> method or #f
;;;      Search delegate(s) as required
(define (find-method selector obj)
  (cond
   [(not (obj? obj)) #f] ; failed
   [((obj-dispatcher obj) selector)] ;; method or #f
   [((obj-dispatcher obj) 'delegate)
    => ;; method which returns delegate(s)
    (lambda (m)
      (let ( (delegate (m obj)) )
        (if (list? delegate)
            ;; multiple inheritance [First Found Rule]
            (let loop ( (delegates delegate) )
              (cond
               [(null? delegates) #f]
               [(find-method selector (car delegates))]
               [else (loop (cdr delegates))]))
            ;; single inheritance
            (find-method selector delegate))))
    ]
   [else #f]
) )



(define (error-not-applicable sym obj)
  ;; create and return a "method"
  (lambda args
        (error sym "not applicable to object" args))
  ;; Note: it would be nice to use (->string obj)
  ;; here, but if there is a bug in obj's ->string
  ;; then we can get an infinite error loop.
)



(define-syntax field-spec->accessor
  (syntax-rules ()
    ([field-spec->accessor (<name> <val>)]
     ;;=>
     (cons '<name> (make-setter-getter <val>))
     )
    ([field-spec->accessor <name>]
     ;;=>
     (cons '<name> (make-setter-getter ':uninitialized)))
    )
)


;;; (object (<field-spec> ...)  <method-spec>... )
(define-syntax object
  (syntax-rules ()
    ([object (<field-spec> ...)
             ((<name> <self> <arg> ...) <exp1> <exp2> ...) ...]
     ;;=>
     [let* ( [fields
              (list (field-spec->accessor <field-spec>) ...)]
             [field-names (map car fields)]
           )
     (make-obj
      (make-dispatch-table
       (append
        fields
        (list ;; method procs
         (cons '<name>
               (lambda (<self> <arg> ...) <exp1> <exp2> ...))
         ...
         ;; Default behaviors not shortcut
         (cons 'field-names  (lambda (obj) field-names))
         (cons 'shallow-clone shallow-clone-method)
         (cons 'deep-clone    deep-clone-method)
        ))))]
     )
) )


(define (make-setter-getter val) ;; return a method
  (case-lambda
    ((obj) val)
    ((obj new-val) (set! val new-val) new-val)))




(define (make-dispatch-table name-method-alist)
  ;; Return a function which maps a symbol [selector]
  ;;  to a method: (lambda (self <arg>...) ...) or #f
  (letrec (
    (find-method ;; NB: does NOT follow delegate
     (lambda (sym)
       (cond
        [(eq? sym 'lookup)
         (lambda (obj) find-method) ;; this function
         ]
        [(assq sym name-method-alist)
         =>  ;; return method
         cdr
         ]
        ;; Default built-in behaviors [keep these few in number]
        [(eq? sym 'method-alist) ;; introspection
         (lambda (obj) name-method-alist)
         ]
        [(eq? sym 'add-method!)
         (lambda (obj name method)
           (cond
            [(assq name name-method-alist)
             => (lambda (pair) (set-cdr! pair method))]
            [else
             (set! name-method-alist
                   (cons (cons name method) name-method-alist))]
            )
           name)
         ]
        [(eq? sym 'remove-method!)
         (lambda (obj to-remove)
           (unless (symbol? to-remove)
             (error 'remove-method!
                    "bad method selector [not a symbol]"
                    to-remove))
           (set! name-method-alist
                 (remp (lambda (pair)
                         (eq? to-remove (car pair)))
                       name-method-alist))
           ;; If method was an accessor,
           ;;   remove it from the field-names list.
           (let* ( (field-names-bucket
                    (assq 'field-names name-method-alist))
                   (field-names-method
                    (cdr field-names-bucket))
                   (field-names-list
                    (field-names-method obj))
                 )
             (when (memq to-remove field-names-list)
               (let ( (new-field-names
                       (remq to-remove field-names-list))
                    )
               (set-cdr! field-names-bucket
                         (lambda (self) field-names-list)))
               ))
           to-remove)
         ]
        [else #f]
        )))
    )
    find-method)
)


(define object? obj?) ;; for export

(define (fresh-alist alist)
  (unless (list? alist)
    (error 'copy-list "requires a list argument" alist))
  (map (lambda (pair) (cons (car pair) (cdr pair))) alist)
)

;;; (shallow-clone obj)
(define (shallow-clone obj)
  ;; don't recursively clone delegate
  (define (clone-accessors alist)
    (map (lambda (pair)
           (cons (car pair)
                 (make-setter-getter ((cdr pair) obj))))
         alist))
  (unless (obj? obj)
    (error 'clone "can't clone non-object" obj))
  (let ( (field-names [$ field-names obj]) )
    (let-values ( [(fields methods)
                   (partition
                    (lambda (pair)
                      (memq (car pair) field-names))
                    [$ method-alist obj])
                   ]
                )
      (make-obj
       (make-dispatch-table
        (append
         (clone-accessors fields)
         (fresh-alist methods)))) 
      ;; fresh-alist used because add-method! uses set-cdr!
  ) )
)

;;; (deep-clone obj)
(define (deep-clone obj)
 ;; like shallow-clone, but DO recursively clone delegate
  (let* ( (cloned (shallow-clone obj))
          (del (assq 'delegate [$ method-alist cloned]))
        )
    (when del ;; (delegate . proc)
      (let* ( (delegate ((cdr del) cloned))
              (cloned-delegate
               (if (list? delegate)
                   (map deep-clone delegate)
                   (deep-clone delegate)))
            )
        (set-cdr! del (lambda (obj) cloned-delegate))))
    cloned)
)

(define shallow-clone-method
  (lambda (self) (shallow-clone self))
)

(define deep-clone-method
  (lambda (self) (deep-clone self))
)


;;; Useful helpers
;;
;;; (define-predicate <pred?>) -- syntax
(define-syntax define-predicate
  (syntax-rules ()
    ((define-predicate <pred?>)
     ;;=>
     (define (<pred?> obj)
       (cond
        [(not (obj? obj)) #f]
        [(find-method '<pred?> obj)
         =>
         (lambda (meth) (meth obj))
         ]
        [else #f]))
     )
) )

;;; (->string thing)  Return a string describing any thing.
(define (->string thing)

  (define (default:obj->string obj)
    ;; assert (obj ?obj)
    (call-with-string-output-port
     (lambda (outp)
       (let ( (field-names ($ field-names  thing))
              (lookup      (obj-dispatcher thing))
              )
         ;; NB does not follow delegates.
         (display "#[instance" outp)
         (for-each (lambda (name)
                     (display " " outp)
                     (display name outp)
                     (display ": " outp)
                     (display (->string ((lookup name) thing))
                              outp))
                   field-names)
         (display "]" outp)))))
  
  (define (default:scheme->string non-object)
    (call-with-string-output-port
     (lambda (outp) (display thing outp))))
  
  ;; ->string main code
  (cond
   [(find-method '->string thing)
    => ;; local object override
    (lambda (m) (m thing))
    ]
   [(obj? thing) (default:obj->string    thing)]
   [else         (default:scheme->string thing)]
) )


)

;;; Optimizaion Note: both the find-method procs (the 2nd is in 
;;; make-dispatch-table) are where method caches would improve 
;;; performance.  This exactly corresponds to the local and
;;; global caches in some Smalltalk implementations.


;;		---   E O F   ---		;;
