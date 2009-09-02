(import (rnrs) (diff))

(apply 
  (lambda (_ file1 file2)
    (let ((file1 (open-input-file file1))
          (file2 (open-input-file file2)))
      (diff-report file1 file2)))
  (command-line))
