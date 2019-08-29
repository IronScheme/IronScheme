#!r6rs
(library (pfds tests sequences)
(export sequences)
(import (rnrs)
        (wak trc-testing)
        (pfds tests utils)
        (pfds sequences))

(define-test-suite sequences
  "Tests for the sequences implementation")
;; Note: at the moment, sequences are a trivial instantiation of
;; fingertrees, and so are pretty much covered by the fingertrees
;; tests.

(define-test-case sequences sequences-bugs
  (let ((s (sequence 'zero 'one 'two)))
    (test-case sequences-bugs ()
      (test-eqv 'zero (sequence-ref s 0))
      (test-eqv 'two (sequence-ref s 2))
      (test-exn assertion-violation? (sequence-ref s -1))
      (test-exn assertion-violation? (sequence-ref s 3)))))

)
