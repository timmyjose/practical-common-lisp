;;;; Chapter 18 - A few FORMAT recipes.

(defun print-list-loop (lst)
  (loop for cons on lst
     do (format t "~a" (car cons))
     when (cdr cons) do (format t ", ")))

(defun print-list-format (lst)
  (format t "~{~a~^, ~}" lst))

(assert (string= (format nil "~d" 1000000) "1000000"))
(assert (string= (format nil "~:d" 1000000) "1,000,000"))
(assert (string= (format nil "~@d" 1000000) "+1000000"))
(assert (string= (format nil "~:@d" 1000000) "+1,000,000"))

;;; basic formatting

(assert (string= (format nil "The value is: ~a" 10) "The value is: 10"))
(assert (string= (format nil "The value is: ~a" "foo") "The value is: foo"))
(assert (string= (format nil "The value is: ~a" (list 1 2 3)) "The value is: (1 2 3)"))

(assert (string= (format nil "~d" 12345) "12345"))
(assert (string= (format nil "~x" 10) "A"))
(assert (string= (format nil "~B" 10) "1010"))
(assert (string= (format nil "~8r" 10) "12"))
(assert (string= (format nil "~f" 1.2345) "1.2345"))
(assert (string= (format nil "~,10f" 1.2345) "1.2345000000"))
(assert (string= (format nil "~$" pi) "3.14"))
(assert (string= (format nil "~r" 12345) "twelve thousand three hundred forty-five"))
(assert (string= (format nil "~:r" 12345) "twelve thousand three hundred forty-fifth"))

(assert (string= (format nil "~@r" 1234) "MCCXXXIV"))
(assert (string= (format nil "~@:r" 1234) "MCCXXXIIII"))

(assert (string= (format nil "file~p" 1) "file"))
(assert (string= (format nil "file~p" 0) "files"))
(assert (string= (format nil "file~p" 2) "files"))

(assert (string= (format nil "~r file~:p" 1) "one file"))
(assert (string= (format nil "~r file~:p" 2) "two files"))
(assert (string= (format nil "~r file~:p" 0) "zero files"))

(assert (string= (format nil "~r famil~:@p" 1) "one family"))
(assert (string= (format nil "~r famil~:@p" 0) "zero families"))
(assert (string= (format nil "~r famil~:@p" 2) "two families"))

;;; conditional formatting

(assert (string= (format nil "~[zero~;one~;two~]" 0) "zero"))
(assert (string= (format nil "~[zero~;one~;two~]" 1) "one"))
(assert (string= (format nil "~[zero~;one~;two~]" 2) "two"))
(assert (string= (format nil "~[zero~;one~;two~]" 10) ""))

(assert (string= (format nil "~[zero~;one~:;two~]" 10) "two")) ; default case provided


