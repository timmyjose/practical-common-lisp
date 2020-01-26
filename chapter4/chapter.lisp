;;; Chapter 4 - Syntax and Semantics

(defun print-list (list)
  (dolist (e list)
    (format t "~a~%" e)))

(defun print-n-times (message n)
  (dotimes (i n)
    (format t "~a~%" message)))
