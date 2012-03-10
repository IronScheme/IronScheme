(package (pfds (0 1))
  (depends (wak-trc-testing))
  (synopsis "Purely Functional Data Structures")
  (description
   "A library of data structures for functional programmers (eventually)."
   "Right now just queues and deques.")
  (homepage "http://github.com/ijp/pfds")
  (libraries
   (sls -> "pfds")
   ("private" -> ("pfds" "private"))))
