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
    (ironscheme strings)
    (ironscheme environment))

  (define dir (path-combine (application-directory) "./lib/srfi/" ))

  (define (fuck)
    (vector-for-each
      (lambda (f)
        (directory-move
          f
          (string-replace f "%3a" "COLON" )))
      (get-directories dir "%3a*"))
    (vector-for-each
      (lambda (f)
        (file-move
          f
          (string-replace f "%3a" "COLON" )))
      (get-files dir "%3a*.*"))
    (vector-for-each
      (lambda (f)
        (file-move
          f
          (string-replace f "%21" "EXCLAIMATION" )))
      (get-files dir "*%21.*"))
    (vector-for-each
      (lambda (f)
        (file-move
          f
          (string-replace f "%2a" "ASTERIX" )))
      (get-files dir "*%2a.*")))

  (define (unfuck)
    (vector-for-each
      (lambda (f)
        (file-move
          f
          (string-replace f "ASTERIX" "%2a" )))
      (get-files dir "*ASTERIX.*"))
    (vector-for-each
      (lambda (f)
        (file-move
          f
          (string-replace f "EXCLAIMATION" "%21" )))
      (get-files dir "*EXCLAIMATION.*"))
    (vector-for-each
      (lambda (f)
        (file-move
          f
          (string-replace f "COLON" "%3a" )))
      (get-files dir "COLON*.*"))
    (vector-for-each
      (lambda (f)
        (directory-move
          f
          (string-replace f "COLON" "%3a" )))
      (get-directories dir "COLON*"))))
