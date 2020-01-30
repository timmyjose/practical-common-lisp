;;;; Chapter 11 - Collections

;;; Arrays and Vectors

(defun literal-vector-example ()
  (let ((v1 (vector))
	(v2 (vector 1 2 3 4 5))
	(v3 (vector 10)))
    (print v1)
    (print v2)
    (print v3)))

(defun make-array-example ()
  (let ((a1 (make-array '(5) :initial-element 99)))
    (print a1)))

(defun fill-pointer-example ()
  (let ((a (make-array '(10) :initial-element 0 :fill-pointer 0)))
    (loop for c across "the quick brown fox jumps over the lazy dog"
       do (vector-push c a))
    (print a)
    (dotimes (i 10)
      (vector-pop a)
      (print a))))

(defun adjustable-vector-example ()
  (let ((v (make-array '(16) :initial-element 99 :adjustable t :fill-pointer 0)))
    (print v)
    (loop for i from 1 upto 20
       do (vector-push-extend i v))
    (print v)
    (loop while (not (zerop (length v)))
       do (vector-pop v)
	 (print v))))

(defun resizable-string-example ()
  (let ((s (make-array '(10) 
		       :fill-pointer 0
		       :adjustable t
		       :element-type 'character)))
    (loop for c across "the quick brown fox jumps over the lazy dog"
       do (vector-push-extend c s))
    (assert (typep s 'string))
    (print s)))

(defun sequence-functions-example ()
  (let ((s "Hello, world")
	(v (make-array '(10) :initial-element 0
		       :fill-pointer 0
		       :adjustable t))
	(l (list 1 2 3 4 5)))
    (assert (= (length s) 12))
    (assert (= (length v) 0))
    (assert (= (length l) 5))
    (assert (char= (elt s 2) #\l))
    (assert (= (elt l 3) 4))
    (loop for i from 1 to 10
	 do (vector-push-extend 99 v))
    (setf (elt v 1) 99)
    (assert (= (elt v 1) 99))))

;;; sequence-iterating functions

(assert (= (count 1 #(1 2 3 4 5 1 2 3)) 2))
(assert (equalp (remove 1 #(1 2 3 4 5 1 2 3)) #(2 3 4 5 2 3)))
(assert (equalp (substitute 10 1 #(1 2 1 2 3 1 2 3 4)) #(10 2 10 2 3 10 2 3 4)))
(assert (= (find 1 #(1 2 3 4 5 1)) 1))
(assert (null (find 100 #(1 2 3 4 5))))
(assert (= (find 1 #(2 3 4 1)) 1))
(assert (= (position 1 #(2 1 3 2 1 4 5)) 1))
(assert (string= (remove #\e "Hello, world") "Hllo, world"))

(assert (= (count "foo" #("foo" "bar" "baz") :test #'string=) 1))
(assert (equalp (find 'c #((a 10) (b 20) (c 30)) :key #'first) '(c 30)))
(assert (equalp (find 'a #((a 10) (b 20) (c 30)) :from-end t :key #'first) '(a 10)))
(assert (equalp (find 'a #((a 10) (b 20) (c 30)) :key #'first) '(a 10)))

(assert (string= (remove #\a "foobarbaz" :count 1) "foobrbaz"))
(assert (string= (remove #\a "foobarbaz" :count 1 :from-end t) "foobarbz"))
(assert (string= (remove #\a "foobarbaz" :count 100) "foobrbz"))
(assert (string= (remove #\a "foobarbaz") "foobrbz"))

(assert (= (count-if #'evenp #(1 2 3 4 5)) 2))
(assert (= (count-if-not #'oddp #(1 2 3 4 5)) 2))
(assert (= (count-if #'oddp #(1 2 3 4 5)) 3))
(assert (= (position-if #'digit-char-p "abcd0001") 4))
(assert (equalp (remove-if-not #'(lambda (x) (char= (elt x 0) #\f)) #("foo" "bar" "foh" "baz" "foom"))
		#("foo" "foh" "foom")))
(assert (= (count-if #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first) 2))
(assert (= (count-if-not #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first) 3))
(assert (equalp (remove-if-not #'alpha-char-p #("foo" "bar" "1baz") :key #'(lambda (x) (elt x 0)))
		#("foo" "bar")))

(assert (equalp (remove-duplicates #(1 2 3 4 1 2 3 4 5)) #(1 2 3 4 5)))
(assert (string-equal (remove-duplicates "hello, world") "he, world"))
(assert (string-equal (remove-duplicates "hello, world" :from-end t) "helo, wrd")) ;; interesting

;;; whole sequence manipulations

(assert (equalp (copy-seq #(1 2 3 4 5)) #(1 2 3 4 5)))
(assert (string= (copy-seq "hello, world") "hello, world"))
(assert (string= (reverse "hello, world") "dlrow ,olleh"))
(assert (equalp (reverse #(1 2 3 4 5)) #(5 4 3 2 1)))

(assert (string= (concatenate 'string "hello, " #(#\w #\o #\r #\l #\d)) "hello, world"))
(assert (equalp (concatenate 'vector #(1 2 3) #(4 5 6)) #(1 2 3 4 5 6)))
(assert (equalp (concatenate 'list (list 1 2 3) '(4 5)) '(1 2 3 4 5)))

;;; sorting and merging

(assert (equalp (sort (vector "foo" "baz" "bar") #'string<)
		#("bar" "baz" "foo")))
(assert (equalp (sort (vector "foo" "bar" "baz") #'string>=)
		#("foo" "baz" "bar")))

;;; not that the following idiom is required for destructive functions like sort, stable-sort, and merge:
;;;
;;; (setf sequence (sort sequence key))
;;;
;;;

(defparameter *numbers* '(1 2 -10 2 4 5 5))

(setf *number* (sort *numbers* #'<))
(assert (equalp *numbers* '(-10 1 2 2 4 5 5)))

(setf *numbers* (reverse *numbers*))

(setf *numbers* (stable-sort *numbers* #'>=))
(assert (equalp *numbers* '(5 5 4 2 2 1 -10)))

