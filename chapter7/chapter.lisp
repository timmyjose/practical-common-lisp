;;;; Chapter 7 - Macros: Standard Control Constructs

;; when and unless

(defun file-in-spam-folder (message)
  (format t "\"~a\" filed in the spam folder.~%" message))

(defun update-spam-database (message)
  (format t "Updated spam database with \"~a\"~%" message))

(defun spam-p (message)
  (cond
    ((> (length message) 5) nil)
    (t t)))

(defun when-demo ()
  (let ((messages '("hello" "world" "how do you do?")))
    (loop for message in messages
       do (when (spam-p message)
	    (file-in-spam-folder message)
	    (update-spam-database message)))))

(defmacro my-when (cond &body body)
  `(if ,cond
       (progn
	 ,@body)))

(defun my-when-demo ()
  (let ((messages '("hello" "world" "how do you do?")))
    (loop for message in messages
       do (my-when (spam-p message)
	    (file-in-spam-folder message)
	    (update-spam-database message)))))

(defmacro my-unless (cond &body body)
  `(if (not ,cond)
       (progn
	 ,@body)))

;;; cond

(defun my-abs (x)
  (cond
    ((< x 0) (- 0 x))
    (t x)))

(assert (= (my-abs 0) 0))
(assert (= (my-abs -100) 100))
(assert (= (my-abs 99) 99))

;;; and, or, and not

(assert (eql (not nil) t))
(assert (eql (not (= 1 1)) nil))
(assert (eql (and (= 1 2) (= 3 3)) nil))
(assert (eql (or (= 1 2) (= 3 3)) t))

;;; looping

;; do, dotimes, and dolist

(defun dolist-demo ()
  (let ((names '(Dave Daphne Deirdre Dermott Diandra)))
    (dolist (name names)
      (format t "~a~%" name))))

(defun dotimes-demo ()
  (let ((numbers '(1 2 3 4 5))
	(sum 0))
    (dotimes (i (length numbers))
      (incf sum (nth i numbers)))
    (assert (= sum 15))
    (format t "Sum = ~d~%" sum)))

(defun dolist-return-demo ()
  (let ((numbers '(1 2 3 4 5)))
    (dolist (num numbers)
      (print num)
      (if (evenp num)
	  (return)))))

(defun dotimes-return-demo ()
  (dotimes (i 10)
    (format t "Hello, ~d~%" i)
    (if (evenp i)
	(format t "~d is even!~%" i))))

(defun times-table (n)
  (dotimes (x n)
    (dotimes (y n)
      (let ((a (1+ x))
	    (b (1+ y)))
	(format t "~d x ~d = ~d~%" a b (* a b))))))

;; (do (variable-definition*)
;;     (end-test-form result-form*)
;;  statement*)

;; where each variable definition is of the form: (var init-form step-form)

(defun nth-fibonacci-number (n)
  (do ((m 0 (1+ m))
       (curr 0 next)
       (next 1 (+ curr next)))
      ((= m n) curr)))

(defun basic-do-example ()
  (do ((i 0 (1+ i)))
      ((= i 10))
    (format t "Hello, ~d~%" i)))

;;; the loop macro

(defun basic-loop-example ()
  (loop for i from 1 to 10
     do (format t "Hello, ~d~%" i)))

(defun do-collect-numbers-into-list (start end)
  (do ((nums nil)
       (i start (1+ i)))
      ((= i (1+ end)) (nreverse nums))
    (push i nums)))

(assert (equal (do-collect-numbers-into-list 1 10) '(1 2 3 4 5 6 7 8 9 10)))

(defun loop-collect-numbers-into-list (start end)
  (loop for i from start to end collecting i))

(assert (equal (loop-collect-numbers-into-list 1 10) '(1 2 3 4 5 6 7 8 9 10)))

(defun sum-of-squares (n)
  (loop for i from 1 to n summing (* i i)))

(assert (= (sum-of-squares 10) 385))

(defun count-vowels-in-string (string)
  (loop for c across string counting (find c "aeiou")))

(assert (= (count-vowels-in-string "the quick brown fox jumps over the lazy dog") 11))

(defun loop-nth-fibonacci-number (n)
  (loop for i below n
     and a = 0 then b
     and b = 1 then (+ b a)
     finally (return a)))

(assert (= (loop-nth-fibonacci-number 10) (nth-fibonacci-number 10)))
