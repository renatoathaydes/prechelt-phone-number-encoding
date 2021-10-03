(defconstant +a-to-z+ "abcdefghijklmnopqrstuvwxyz")

(defun random-word-ext ()
  (let* ((ext-length (random 8))
         (chars (loop for c from 1 to ext-length
                      collecting (aref +a-to-z+ (random #.(length +a-to-z+))))))
    (map 'string 'identity chars)))

(defun maybe-make-longer (word)
  (if (< (random 1.0) 0.5)
      (format nil "~a~a" word (random-word-ext))
      word))

(defun make-words-longer (in-file out-file)
  "With a certain probability, add random chars to short words to make them longer on average."
  (with-open-file (in in-file)
    (with-open-file (out out-file :direction :output :if-exists :supersede)
      (loop for word = (read-line in nil) while word do
        (let ((actual-word (maybe-make-longer word)))
          (format out "~a~%" actual-word))))))

(let ((args (rest sb-ext:*posix-argv*)))
  (if (= 2 (length args))
      (make-words-longer (first args) (second args))
      (error "expected two arguments")))
