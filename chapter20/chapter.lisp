;;;; Chapter 20 - The Special Operators

;;; controlling evaluation - quote, if, progn.

;;; manipulating the lexical environment - let, let*, setq, flet, labels, macrolet, symbol-macrolet.

(defun factorial (n)
  (labels ((f (n acc)
	     (cond
	       ((zerop n) acc)
	       (t (f (1- n) (* acc n))))))
    (f n 1)))

(assert (= (factorial 10) 3628800))

(defun len (lst)
  (labels ((f (lst acc)
	     (cond
	       ((null lst) acc)
	       (t (f (cdr lst) (1+ acc))))))
    (f lst 0)))

(assert (= (len '(1 2 3 4 5)) 5))

(defun square (x)
  (flet ((mult (x y)
	   (* x y)))
    (mult x x)))

(assert (= (square 10) 100))

;;; local flow of control - block, return-from, tagbody, go.

(defun tagbody-go-example ()
  (tagbody
   top
     (print 'hello)
     (when (>= 8 (random 10)) (go top))))

(defun silly-tagbody-go-example ()
  (tagbody
   a
     (print 'a)
     (if (zerop (random 2))
	 (go c))
   b
     (print 'b)
     (if (zerop (random 2))
	 (go a))
   c
     (print 'c)
     (if (zerop (random 2))
	 (go a))))

;;; unwinding the stack - unwind-protect, catch, throw.

(defun foo ()
  (format t "Entering foo~%")
  (block a
    (format t "Entering block a~%")
    (bar #'(lambda () (return-from a))) ; non-local return
    (format t "Leaving block a~%"))
  (format t "Leaving foo~%"))

(defun bar (fn)
  (format t "Entering bar~%")
  (baz fn)
  (format t "Leaving bar~%"))

(defun baz (fn)
  (format t "Entering baz~%")
  (block a ; this makes no difference due to lexical binding
    (funcall fn))
  (format t "Leaving baz~%"))

;; note that catch and throw are the dynamic counterparts of block and return-from.

(defparameter *obj* (cons nil nil))

(defun one ()
  (format t "Entering one~%")
  (catch *obj*
    (two))
  (format t "Leaving one~%"))

(defun two ()
  (format t "Entering two~%")
  (three)
  (format t "Leaving two~%"))

;; since catch and throw are dynamic, this version would
;; have completely different behaviour than the function three
(defun three-prime ()
  (format t "Entering three~%")
  (catch *obj*
    (throw *obj* nil))
  (format t "Leaving three~%"))

(defun three ()
  (format t "Entering three~%")
  (throw *obj* nil)
  (format t "Leaving three~%"))

(defun good-function (n)
  (* n n ))

(define-condition something-awful (error)
  ((text
    :initarg :text
    :reader text)))

(defun bad-function (n)
  (error 'something-awful :text "Something awful indeed!"))

(defun unwind-protect-example ()
  (assert (= (unwind-protect (good-function 10)
	       (format t "Cleaned up after good function~&")) 100))
  (assert (= (unwind-protect (handler-case (bad-function 10)
			       (something-awful () 0))
	       (format t "Cleaned up after bad function~%"))
	     0)))

(defmacro with-database-connection ((var &rest open-args) &body body)
  `(Let ((,var (open-connection ,@open-args)))
     (unwind-protect (progn ,@body)
       (close-connection ,var))))

;; multiple values - multiple-value-call

(multiple-value-bind (x y z) (values 1 2 3)
  (assert (= (+ x y z) 6)))

(multiple-value-bind (x y z) (values-list '(1 2 3))
  (assert (= (+ x y z) 6)))

(assert (= (multiple-value-call #'+ (values 1 2) (values 3 4)) 10))
(assert (equalp (values-list (multiple-value-list (values 1 2))) (values 1 2)))

;;; eval-when - :compile-toplevel, :load-toplevel, :execute

;;; miscellenaous special operators - load-time-value, locally, the, and progv.





