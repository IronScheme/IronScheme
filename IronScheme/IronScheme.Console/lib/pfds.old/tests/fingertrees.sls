#!r6rs
(library (pfds tests fingertrees)
(export fingertrees)
(import (rnrs)
        (wak trc-testing)
        (pfds tests utils)
        (rename (pfds fingertrees)
                (make-fingertree %make-fingertree)
                (list->fingertree %list->fingertree))
        )

;; Right now, I am not testing the monoidal parts of fingertrees, so
;; we use constructor that replaces these with arbitrary values
(define (make-fingertree)
  (%make-fingertree 0 (lambda (x y) x) (lambda (x) x)))

(define (list->fingertree l)
  (%list->fingertree l 0 (lambda (x y) x) (lambda (x) x)))

(define (list->product-tree l)
  (%list->fingertree l 1 * values))

(define (list->last-tree l)
  (define *cookie* (cons 'no 'last))
  (define (pick x y)
    (if (eq? *cookie* y)
        x
        y))
  (%list->fingertree l *cookie* pick values))

(define-test-suite fingertrees
  "Tests for the fingertree implementation")

(define-test-case fingertrees empty-tree ()
  (test-predicate fingertree? (make-fingertree))
  (test-predicate fingertree-empty? (make-fingertree)))

(define-test-case fingertrees construction
  (let ((l1 '(a b c d e f))
        (l2 '((#t . f) (#t . e) (#t . d) (#t . c) (#t . b) (#t . a)))
        (l3 '((#f . a) (#f . b) (#f . c) (#f . d) (#f . e) (#f . f)))
        (l4 '((#f . b) (#f . c) (#t . a) (#f . d) (#f . e) (#f . f)))
        (l5 '((#f . e) (#t . d) (#t . c) (#t . b) (#f . f) (#t . a)))
        (make (lambda (alist)
                (fold-left (lambda (tree pair)
                             (if (car pair)
                                 (fingertree-cons (cdr pair) tree)
                                 (fingertree-snoc tree (cdr pair))))
                           (make-fingertree)
                           alist)))
        (empty (make-fingertree)))
    (test-case construction ()
      (test-eqv #f (fingertree-empty? (fingertree-cons #f empty)))
      (test-eqv #f (fingertree-empty? (fingertree-snoc empty #f)))
      (test-equal l1 (fingertree->list (make l2)))
      (test-equal l1 (fingertree->list (make l3)))
      (test-equal l1 (fingertree->list (make l4)))
      (test-equal l1 (fingertree->list (make l5))))))

(define-test-case fingertrees removal
  (let* ((l1 '(a b c d e f))
         (f1 (list->fingertree l1))
         (f2 (make-fingertree)))
    (test-case removal ()
      (test-exn fingertree-empty-condition? (fingertree-uncons f2))
      (test-exn fingertree-empty-condition? (fingertree-unsnoc f2))
      (let-values (((head tail) (fingertree-uncons f1)))
        (test-eqv (car l1) head)
        (test-equal (cdr l1) (fingertree->list tail)))
      (let*-values (((init last) (fingertree-unsnoc f1))
                    ((l*) (reverse l1))
                    ((l1-last) (car l*))
                    ((l1-init) (reverse (cdr l*))))
        (test-eqv l1-last last)
        (test-equal l1-init (fingertree->list init))))))

(define-test-case fingertrees conversion
  (let ((l1 '(31 238 100 129 6 169 239 150 96 141 207 208 190 45 56
              183 199 254 78 210 14 131 10 220 205 203 125 111 42 249))
        (l2 '(25 168 21 246 39 211 60 83 103 161 192 201 31 253
              156 218 204 186 155 117)))
    (test-case conversion ()
      (test-equal '() (fingertree->list (list->fingertree '())))
      (test-equal l1 (fingertree->list (list->fingertree l1)))
      (test-equal l2 (fingertree->list (list->fingertree l2))))))

(define-test-case fingertrees ftree-append
  (let ((l1 '(31 238 100 129 6 169 239 150 96 141 207 208 190 45 56
              183 199 254 78 210 14 131 10 220 205 203 125 111 42 249))
        (l2 '(25 168 21 246 39 211 60 83 103 161 192 201 31 253
              156 218 204 186 155 117))
        (append* (lambda (a b)
                   (fingertree->list
                    (fingertree-append
                     (list->fingertree a)
                     (list->fingertree b))))))
    (test-case ftree-append ()
      (test-equal (append l1 '()) (append* l1 '()))
      (test-equal (append '() l1) (append* '() l1))
      (test-equal (append l1 l2) (append* l1 l2))
      (test-equal (append l1 l1) (append* l1 l1))
      (test-equal (append l1 l2) (append* l1 l2)))))

(define-test-case fingertrees monoidal-operation
  (let ((l1 '(31 238 100 129 6 169 239 150 96 141
              207 208 190 45 56 183 199 254 78 210))
        (l2 '((31 238 100 129 6) (169 239 150) (96 141 207 208 190)
              ()  (45 56 183 199) (254 78 210)))
        (car/default (lambda (dflt) (lambda (x) (if (pair? x) (car x) dflt))))
        (list->sum-tree (lambda (l1) (%list->fingertree l1 0 + values))))
    (test-case moniodal-operation ()
      (test-equal 254 (fingertree-measure (%list->fingertree l1 0 max values)))
      (test-equal 6 (fingertree-measure (%list->fingertree l1 1000 min values)))
      (test-equal l1 (fingertree-measure (%list->fingertree l2 '() append values)))
      (test-equal 595 (fingertree-measure
                       (%list->fingertree l2 0 + (car/default 0))))
      ;; sum of l1 is 4239
      (test-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 0))
                                                           (list->sum-tree l1))))
                       (fingertree->list (fingertree-append a b))))
      (test-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 1000))
                                                           (list->sum-tree l1))))
                       (fingertree->list (fingertree-append a b))))
      (test-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 2000))
                                                           (list->sum-tree l1))))
                       (fingertree->list (fingertree-append a b))))
      (test-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 5000))
                                                           (list->sum-tree l1))))
                       (fingertree->list (fingertree-append a b)))))))

(define-test-case fingertrees fingertree-folds
  (let* ((l '(31 238 100 129 6 169 239 150 96 141
                 207 208 190 45 56 183 199 254 78 210))
         (lrev (reverse l))
         (total (apply + l))
         (ft (list->fingertree l)))
    (test-case fingertree-folds ()
      ;; empty case
      (test-eqv #t (fingertree-fold (lambda _ #f) #t (make-fingertree)))
      (test-eqv #t (fingertree-fold-right (lambda _ #f) #t (make-fingertree)))
      ;; associative operations
      (test-eqv total (fingertree-fold + 0 ft))
      (test-eqv total (fingertree-fold-right + 0 ft))
      ;; non-associative operations
      (test-equal lrev (fingertree-fold cons '() ft))
      (test-equal l (fingertree-fold-right cons '() ft)))))

(define-test-case fingertrees reversal
  (let ((rev (lambda (l)
               (fingertree->list
                (fingertree-reverse (list->fingertree l)))))
        (id (lambda (l)
              (fingertree->list
               (fingertree-reverse
                (fingertree-reverse (list->fingertree l))))))
        (l1 '(126 6 48 86 2 119 233 92 230 160))
        (l2 '(25 168 21 246 39 211 60 83 103 161
              192 201 31 253 156 218 204 186 155 117)))
    (test-case reversal ()
      ;; behaves the same as regular reverse on lists
      (test-eqv '() (rev '()))
      (test-equal '(1) (rev '(1)))
      (test-equal '(6 5 4 3 2 1) (rev '(1 2 3 4 5 6)))
      (test-equal (reverse l1) (rev l1))
      (test-equal (reverse l2) (rev l2))
      ;; double reversal is the the same list
      (test-equal l1 (id l1))
      (test-equal l2 (id l2))
      ;; a fingertree will have the same measure as its reverse if
      ;; the monoid is commutative
      (test-equal (fingertree-measure (list->product-tree l1))
                  (fingertree-measure
                   (fingertree-reverse (list->product-tree l1))))
      ;; otherwise they are not necessarily the same
      ;; in this case, they are the same only if the first and last
      ;; elements are the same
      (test-not
       (equal? (fingertree-measure (list->last-tree l2))
               (fingertree-measure (fingertree-reverse (list->product-tree l2))))))))

)
