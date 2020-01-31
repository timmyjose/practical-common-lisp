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

;;; note that the following idiom is required for destructive functions like sort, stable-sort, and merge:
;;;
;;; (setf sequence (sort sequence key))
;;;
;;;

(defparameter *numbers* '(1 2 -10 2 4 5 5))

(setf *numbers* (sort *numbers* #'<))
(assert (equalp *numbers* '(-10 1 2 2 4 5 5)))

(setf *numbers* (reverse *numbers*))

(setf *numbers* (stable-sort *numbers* #'>=))
(assert (equalp *numbers* '(5 5 4 2 2 1 -10)))

;;; subsequence manipulations

(assert (string= (subseq "Hello, world"  0 12) "Hello, world"))
(assert (string= (subseq "Hello, world" 4 8) "o, w"))
(assert (string= (subseq "Hello, world" 0) "Hello, world"))

(defparameter *x* (copy-seq "foobarbaz"))

(setf (subseq *x* 3 6) "xxx")
(assert (string= *x* "fooxxxbaz"))

(setf (subseq *x* 3 6) "abcd")
(assert (string= *x* "fooabcbaz"))

(setf (subseq *x* 3 6) "xx")
(assert (string= *x* "fooxxcbaz"))

(assert (= (search "bar" "foobarbaz") 3))
(assert (= (position #\b "foobarbaz") 3))
(assert (= (mismatch "foobarbaz" "foom") 3))
(assert (null (mismatch "hello" "hello")))

;;; sequence predicates

(assert (every #'evenp #(2 4 6 8 10)))
(assert (some #'evenp #(1 2 3 4 5)))
(assert (notevery #'evenp #(1 2 3 4 5)))
(assert (notany #'oddp '(2 4 6 8 10)))

;; note: pairwise from the sequences in this case
(assert (every #'> #(2 4 6 8) #(1 2 3 5)))
(assert (notevery #'> #(2 3 4 5) #(3 4 5 6)))
(assert (notany #'< #(5 6 7 8) #(1 2 3 4)))
(assert (some #'> #(1 2 3 4) #(0 3 4 5)))

;;; sequence-mapping functions

(assert (equalp (map 'vector #'* #(1 2 3 4 5) #(5 4 3 2 1))
		#(5 8 9 8 5)))

(assert (equalp (mapcar #'+ '(1 2 3 4 5) '(5 4 3 2 1))
		'(6 6 6 6 6)))

(assert (= (reduce #'+ (loop for i from 1 to 10 collecting i)) 55))
(assert (= (reduce #'* (loop for i from 1 to 10 collecting i)) 3628800))
(assert (= (reduce #'+ #(1 2 3 4 5 6 7 8 9 10) :initial-value 100) 155))

;;; hash tables

(defun hash-table-example ()
  (let ((h (make-hash-table :test #'eql)))
    (assert (null (gethash 'foo h)))
    (setf (gethash 'foo h) 'bar)
    (assert (eql 'bar (gethash 'foo h)))))

;; note that gethash returns multiple values - the first one is the value of the retrieval call, and the second
;; is whether the value was present or not - useful when the value returned is nil.

(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
	(format nil "Key ~a was present in the hash table with value ~a" key value)
	(format nil "Key ~a was not present in the hash table" key))))

(defun extended-hash-table-example ()
  (let ((h (make-hash-table :test #'eql)))
    (format t (show-value 'foo h))
    (terpri)
    (setf (gethash 'foo h) 'bar)
    (format t (show-value 'foo h))
    (terpri)
    (setf (gethash 'bar h) nil)
    (format t (show-value 'bar h))
    (terpri)
    (remhash 'bar h)
    (format t (show-value 'bar h))
    (maphash #'(lambda (k v) (format t "~a: ~a~%" k v)) h)
    (clrhash h)))

(defun display-hash-example ()
  (let ((h (make-hash-table :test #'equal)))
    (loop for word in '("hello" "world" "nice" "to" "meet" "you")
       do (setf (gethash word h) (length word)))
    (maphash #'(lambda (k v) (format t "~a: ~a~%" k v)) h)))

(defun display-hash-using-loop-example ()
  (let ((phone-book (make-hash-table :test #'equal)))
    (setf (gethash "bob" phone-book) "123-4567")
    (setf (gethash "alice" phone-book) "987-6543")
    (setf (gethash "carter" phone-book) "434-1214")
    (loop for name being the hash-keys in phone-book using (hash-value phone)
       do (format t "~a: ~a~%" name phone))))
