;; Peter Norvig - Programming Challange from Erann Gat:
;; http://www.flownet.com/ron/papers/lisp-java/
;; Given a list of words and a list of phone numbers, find all the ways that
;; each phone number can be expressed as a list of words.

;; Run: (main "word-list-file-name" "phone-number-file-name")

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setq *block-compile-default* t)

;; (declaim (inline nth-digit nth-digit-string char->digit))

(declaim (ftype (function (simple-string (unsigned-byte 8)) (bit-vector)) nth-digit))
(defun nth-digit (digits i)
  "The i-th element of a character string of digits, as a bit-vector representing 0 to 9."
  (ecase (char digits i)
    (#\0 #*0000)
    (#\1 #*0001)
    (#\2 #*0010)
    (#\3 #*0011)
    (#\4 #*0100)
    (#\5 #*0101)
    (#\6 #*0110)
    (#\7 #*0111)
    (#\8 #*1000)
    (#\9 #*1001)))

(declaim (ftype (function (simple-string (unsigned-byte 8)) simple-string) nth-digit-string))
(defun nth-digit-string (digits i)
  "The i-th element of a character string of digits, as an integer 0 to 9."
  (string (char digits i)))

(eval-when (:compile-toplevel)
  (defun mapchar (&rest chars)
    "Map each char in chars to its char-code and that of its upper-case."
    (loop for ch in chars
          collect (char-code ch)
          collect (char-code (char-upcase ch)))))

(declaim (ftype (function ((unsigned-byte 8)) (bit-vector)) byte->digit))
(defun byte->digit (b)
  "Convert a byte (alphabetic ASCII) to a bit-vector representing a digit according
   to the phone number rules."
  (ecase b
    (#.(mapchar #\e)         #*0000)
    (#.(mapchar #\j #\n #\q) #*0001)
    (#.(mapchar #\r #\w #\x) #*0010)
    (#.(mapchar #\d #\s #\y) #*0011)
    (#.(mapchar #\f #\t)     #*0100)
    (#.(mapchar #\a #\m)     #*0101)
    (#.(mapchar #\c #\i #\v) #*0110)
    (#.(mapchar #\b #\k #\u) #*0111)
    (#.(mapchar #\l #\o #\p) #*1000)
    (#.(mapchar #\g #\h #\z) #*1001)))

(declaim (ftype (function ((unsigned-byte 8)) boolean) alpha-byte-p))
(defun alpha-byte-p (b)
  (or
   (and (>= b #.(char-code #\a)) (<= b #.(char-code #\z)))
   (and (>= b #.(char-code #\A)) (<= b #.(char-code #\Z)))))

(declaim (ftype (function (simple-string) boolean) digitp))
(defun digitp (s)
  (if (and (eq 1 (length s))
           (let ((ch (elt s 0))) (digit-char-p ch)))
      t
      nil))

(declaim (ftype (function (simple-string) (bit-vector)) word->number))
(defun word->number (word)
  "Translate a word (string) into a phone number, according to the rules."
  (let ((n (make-sequence 'bit-vector #.(* 25 8) :initial-element 1))
        (word-bytes (string-to-octets word)))
    (loop for i from 0 below (length word-bytes)
          for b = (elt word-bytes i)
          with byte-start = 0
          with byte-end = 4
          when (alpha-byte-p b) do
            (setf (subseq n byte-start byte-end) (byte->digit b)
                  byte-start byte-end
                  byte-end (+ byte-end 4)))
    n))

(defglobal *dict* nil
  "A hash table mapping a phone number (integer) to a list of words from the
  input dictionary that produce that number.")

(declaim (ftype (function (simple-string simple-string &optional (unsigned-byte 8) list)) print-translations))
(defun print-translations (num digits &optional (start 0) (words nil))
  "Print each possible translation of NUM into a string of words.  DIGITS
  must be WORD with non-digits removed.  On recursive calls, START is the
  position in DIGITS at which to look for the next word, and WORDS is the list
  of words found for (subseq DIGITS 0 START).  So if START gets to the end of
  DIGITS, then we have a solution in WORDS.  Otherwise, for every prefix of
  DIGITS, look in the dictionary for word(s) that map to the value of the
  prefix (computed incrementally as KEY), and for each such word try to extend
  the solution with a recursive call.  There are two complications: (1) the
  rules say that in addition to dictionary words, you can use a single
  digit in the output, but not two digits in a row. Also (and this seems
  silly) you can't have a digit in a place where any word could appear.
  I handle this with the variable FOUND-WORD; if it is false after the loop,
  and the most recent word is not a digit, try a recursive call that pushes a
  digit. (2) The other complication is that the obvious way of mapping
  strings to integers would map R to 2 and ER to 02, which of course is
  the same integer as 2.
  Instead of using integers, we use BIT-VECTOR because that allows us to
  efficiently compute and compare them."
  (if (>= start (length digits))
      (format t "~a:~{ ~a~}~%" num (reverse words))
      (let ((found-word nil)
            (key (make-sequence 'bit-vector #.(* 25 8) :initial-element 1)))
        (loop for i from start below (length digits)
              for key-start from 0 by 4 and key-end from 4 by 4 do
                (setf (subseq key key-start key-end) (nth-digit digits i))
                (loop for word in (gethash key *dict*) do
                  (setf found-word t)
                  (print-translations num digits (1+ i) (cons word words))))
        (when (and (not found-word) (not (digitp (first words))))
          (print-translations num digits (1+ start)
                              (cons (nth-digit-string digits start) words))))))

(defun load-dictionary (file size)
  "Create a hashtable from the file of words (one per line).  Takes a hint
  for the initial hashtable size.  Each key is the phone number for a word;
  each value is a list of words with that phone number."
  (let ((table (make-hash-table :test #'equal :size size)))
    (with-open-file (in file :external-format :ASCII)
      (loop for word = (read-line in nil) while word do
        (push word (gethash (word->number word) table))))
    table))

(defun main (&optional (dict "tests/words.txt") (nums "tests/numbers.txt") (dict-size 100))
  "Read the input file ¨DICT and load it into *dict*.  Then for each line in
  NUMS, print all the translations of the number into a sequence of words,
  according to the rules of translation."
  (setf *dict* (load-dictionary dict dict-size))
  (with-open-file (in nums)
    (loop for num = (read-line in nil) while num do
          (print-translations num (remove-if-not #'digit-char-p num)))))

(apply #'main (cdr sb-ext:*posix-argv*))
