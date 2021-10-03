(defun find-words-larger-than (min-length file)
  (with-open-file (in file)
    (loop for word = (read-line in nil) while word do
      (when (> (length word) min-length)
        (format t "~a: ~a~%" word (length word))))))

(let ((args (rest sb-ext:*posix-argv*)))
  (if (= 2 (length args))
      (find-words-larger-than (read-from-string (first args)) (second args))
      (error "expected one argument")))
