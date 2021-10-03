(defglobal *counts* (make-hash-table :size 36))
(defglobal *max-length* 1)

(defun count-lengths-in-file (file)
  "Count the length of each word in the file"
  (with-open-file (in file)
    (loop for word = (read-line in nil) while word do
      (setf *max-length* (max (length word) *max-length*))
      (incf (gethash (length word) *counts* 0)))))

(defun print-counts ()
  "Print the counts for the lengths of words in the file"
  (loop for i from 1 to *max-length* do
    (let* ((count (gethash i *counts* 0)))
      (format t "~a: ~a~%" i count))))

(let ((args (rest sb-ext:*posix-argv*)))
  (if (= 1 (length args))
      (progn
        (count-lengths-in-file (first args))
        (print-counts))
      (error "expected one argument")))
