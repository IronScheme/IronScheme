#| License
Copyright (c) 2024 Llewellyn Pritchard
All rights reserved.
This source code is subject to terms and conditions of the BSD License.
See docs/license.txt. |#
(library
  (nuget)
  (import
    (ironscheme)
    (ironscheme files)
    (ironscheme strings))
  (export
    fuck
    unfuck)

  (define (fuck)
    (for-each
      (lambda (f)
        (directory-move
          f
          (string-replace f "%3a" "COLON" ))
      (get-directories "*%3a*")))
    (for-each
      (lambda (f)
        (file-move
          f
          (string-replace f "%3a" "COLON" ))
      (get-files "*%3a*"))))

  (define (unfuck)
    (for-each
      (lambda (f)
        (file-move
          f
          (string-replace f "COLON" "%3a" ))
      (get-files "*COLON*")))
    (for-each
      (lambda (f)
        (directory-move
          f
          (string-replace f "COLON" "%3a" ))
      (get-directories "*COLON*")))))



