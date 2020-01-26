;;;; Chapter 6 - Variables

(defun lexical-shadowing (x)
  (format t "Paramater x is ~a~%" x)
  (let ((x 2))
    (format t "Outer LET's x is ~a~%" x)
    (let ((x 3))
      (format t "INNER LET's x is ~a~%" x))
    (format t "OUTER LET's x is (again) ~a~%" x))
  (format t "Parameter x is (again) ~a~%" x))

(defun dotimes-binding ()
  (dotimes (x 10)
    (format t "Hello, ~d~%" x)))

;;; Lexical variables and Closures

(defun basic-closure-demo ()
  (let ((closure (get-closure)))
    (dotimes (i 10)
      (format t "~d " (funcall closure)))))

(defun get-closure ()
  (let ((count 0))
    #'(lambda ()
	(setf count (+ 1 count)))))

(defun multiple-bindings-closures ()
  (multiple-value-bind (inc-closure dec-closure closure) (get-multiple-bindings-closures)
    (dotimes (i 10)
      (format t "~d " (funcall inc-closure)))
    (terpri)
    (dotimes (j 10)
      (format t "~d " (funcall dec-closure)))
    (terpri)
    (dotimes (k 10)
      (format t "~d " (funcall closure)))))

(defun get-multiple-bindings-closures ()
  (let ((count 0))
    (values
     #'(lambda () (incf count))
     #'(lambda () (decf count))
     #'(lambda () count))))

;;; Dynamic/Special variables

(defvar *x* 100)

(format t "~d~%" *x*) ; 100

(let ((*x* 200))
  (format t "~d~%" *x*)) ; 200

(format t "~d~%" *x*) ; 100

(defun print-message (message)
  (with-standard-io-syntax 
    (format *standard-output* message)))

;; standard output here, in this usage, has been rebound to
;; a file stream
(defun dynamic-variables-example ()
  (with-open-file (out "output.out"
		       :direction :output
		       :if-exists :supersede)
    (let ((*standard-output* out))
      (print-message "Hola, Mundo!"))))

;; this would be printed to standard output since
;; *standard-output* is no longer bound to the file stream
(print-message "Hallo, Welt!")

;;; Constants

(defconstant +MYCONSTANT+ 1.2345)

(defun use-my-constant (n)
  (print (+ n 100)))



