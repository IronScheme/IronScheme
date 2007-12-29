(import (rnrs))

(library (stack)
  (export make push! pop! empty!)
  (import (rnrs)
          (rnrs mutable-pairs))

  (define (make)
    (list '()))

  (define (push! s v)
    (set-car! s (cons v (car s))))

  (define (pop! s)
    (let ((v (caar s))) (set-car! s (cdar s)) v))

  (define (empty! s)
    (set-car! s '())))

(library (balloons)
  (export make push pop)
  (import (rnrs))

  (define (make w h)
    (cons w h))

  (define (push b amt)
    (cons (- (car b) amt) (+ (cdr b) amt)))

  (define (pop b)
    (display "Boom! ")
    (display (* (car b) (cdr b)))
    (newline)))


(library (party)
  (export (rename (balloon:make make) (balloon:push push))
          push! make-party
          (rename (party-pop! pop!)))
  (import (rnrs)
          (only (stack) make push! pop!) ;; not empty!
          (prefix (balloons) balloon:))

  ;; Creates a party as a stack of balloons, starting with
  ;; two balloons
  (define (make-party)
    (let ((s (make))) ;; from stack
      (push! s (balloon:make 10 10))
      (push! s (balloon:make 12 9)) s))

  (define (party-pop! p)
    (balloon:pop (pop! p))))
	
	
(import (party))

(define p (make-party))
(pop! p) ; displays "Boom! 108"
(push! p (push (make 5 5) 1))
(pop! p) ; displays "Boom! 24	


