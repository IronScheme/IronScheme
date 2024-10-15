#| License
Copyright (c) 2024 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#
(library
  (nuget)
  (export
    fuck
    unfuck)
  (import
    (ironscheme)
    (ironscheme files)
    (ironscheme strings))

  (define (fuck)
    (vector-for-each
      (lambda (f)
        (directory-move
          f
          (string-replace f "%3a" "COLON" )))
      (get-directories "./lib/srfi/" "%3a*"))
    (vector-for-each
      (lambda (f)
        (file-move
          f
          (string-replace f "%3a" "COLON" )))
      (get-files "./lib/srfi/" "%3a*.*")))

  (define (unfuck)
    (vector-for-each
      (lambda (f)
        (file-move
          f
          (string-replace f "COLON" "%3a" )))
      (get-files "./lib/srfi/" "COLON*.*"))
    (vector-for-each
      (lambda (f)
        (directory-move
          f
          (string-replace f "COLON" "%3a" )))
      (get-directories "./lib/srfi/" "COLON*"))))
