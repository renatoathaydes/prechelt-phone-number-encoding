(defglobal *counts* (make-hash-table :size 36))

(defun count-chars (word)
  "Counts each different alphabetical character in the given word"
  (loop for c across word
        when (alpha-char-p c) do
          (incf (gethash (char-downcase c) *counts* 0))))

(defun count-chars-in-file (file)
  (with-open-file (in file)
    (loop for word = (read-line in nil) while word do
      (count-chars word))))

(defun print-counts ()
  "Print the counts for each char"
  (loop for c from (char-code #\a) to (char-code #\z) do
    (let* ((ch (code-char c))
           (count (gethash (char-downcase (code-char c)) *counts* 0)))
      (format t "~a: ~a~%" ch count))))

(let ((args (rest sb-ext:*posix-argv*)))
  (if (= 1 (length args))
      (progn
        (count-chars-in-file (first args))
        (print-counts))
      (error "expected one argument")))

  
