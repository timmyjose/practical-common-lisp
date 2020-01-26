;;;; Chapter 5 - Functions.

(defun hello-world ()
  "This function simply prints, \"Hello, world\"."
  (format t "Hello, world~%"))

(defun verbose-sum (x y)
  "Returns the sum of two numbers after printing a message."
  (format t "Summing ~d and ~d~%" x y)
  (+ x y))

;;; optional parameters

(defun optional-demo (a b &optional c d)
  (list a b c d))

(assert (equal (optional-demo 1 2 3 4) '(1 2 3 4)))
(assert (equal (optional-demo 1 2) '(1 2 nil nil)))
(assert (equal (optional-demo 1 2 3) '(1 2 3 nil)))

(defun optional-default-demo (a &optional (b 10))
  (list a b))

(assert (equal (optional-default-demo 1 2) '(1 2)))
(assert (equal (optional-default-demo 1) '(1 10)))

(defun optional-default-supplied-demo (a b &optional (c 10 c-supplied-p))
  (list a b c c-supplied-p))

(assert (equal (optional-default-supplied-demo 1 2 3) '(1 2 3 t)))
(assert (equal (optional-default-supplied-demo 1 2) '(1 2 10 nil)))
(assert (equal (optional-default-supplied-demo 1 2 10) '(1 2 10 t)))

;;; rest parameters

(defun add-numbers (&rest numbers)
  (apply #'+ numbers))

(assert (= (add-numbers 1 2 3 4 5) 15))
(assert (= (add-numbers 1) 1))
(assert (= (add-numbers) 0))

(defun varargs-demo (a b &optional (c 3 c-supplied-p) &rest args)
  (list a b c c-supplied-p args))

(assert (equal (varargs-demo 1 2 3 4 5) '(1 2 3 t (4 5))))
(assert (equal (varargs-demo 1 2 3 4) '(1 2 3 t (4))))
(assert (equal (varargs-demo 1 2 3) '(1 2 3 t nil)))
(assert (equal (varargs-demo 1 2) '(1 2 3 nil nil)))

;;; keyword parameters - come after any required, optional, and rest parameters.

(defun keyword-demo (&key a b c)
  (list a b c))

(assert (equal (keyword-demo :a 1 :b 2 :c 3) '(1 2 3)))
(assert (equal (keyword-demo :c 3 :a 1) '(1 nil 3)))
(assert (equal (keyword-demo) '(nil nil nil)))
(assert (equal (keyword-demo :b 2) '(nil 2 nil)))
(assert (equal (keyword-demo :c 3) '(nil nil 3)))

(defun extended-keyword-demo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b b-supplied-p c))

(assert (equal (extended-keyword-demo :a 1 :b 2 :c 3) '(1 2 t 3)))
(assert (equal (extended-keyword-demo :c 10) '(0 0 nil 10)))
(assert (equal (extended-keyword-demo) '(0 0 nil 0)))

(defun final-keyword-demo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))

(assert (equal (final-keyword-demo :apple 10 :box 20 :charlie 30) '(10 20 30 t)))
(assert (equal (final-keyword-demo :charlie 100) '(nil 0 100 t)))
(assert (equal (final-keyword-demo :box 20) '(nil 20 0 nil)))

;;; Mixing different parameter types

(defun interesting-demo (&rest rest &key a b c)
  (list rest a b c))

(assert (equal (interesting-demo :a 1 :c 3 :b 2) '((:a 1 :c 3 :b 2) 1 2 3)))

;;; return-from

(defun return-from-demo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (if (> (* i j) n)
	  (return-from return-from-demo (list i j))))))

;;; Higher-order functions

(defun plot (fn min max step)
  (loop for i from min to max by step do
       (loop repeat (funcall fn i) do (format t "*"))
       (format t "~%")))

;;; anonymous functions

(defparameter *numbers* '(1 2 -10 29 1 2 3 2 1))

(let ((sorted-numbers (sort *numbers* #'(lambda (x y) (<= x y)))))
  (assert (equal sorted-numbers '(-10 1 1 1 2 2 2 3 29))))








