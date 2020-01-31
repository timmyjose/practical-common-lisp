;;;; Chapter 12 - Lists

(defparameter *cons* (cons 1 2))

(setf (car *cons*) 100)
(setf (cdr *cons*) 20)
(assert (= (car *cons*) 100))
(assert (= (cdr *cons*) 20))

(defparameter *list* (list 1 2 3 4 5))
(assert (= (first *list*) 1))
(assert (equalp (rest *list*) '(2 3 4 5)))
(assert (= (second *list*) 2))
(assert (= (third *list*) 3))
(assert (= (fourth *list*) 4))
(assert (= (fifth *list*) 5))

;;; functional programming and lists

(defparameter *x* (list 1 2 3))
(nconc *x* (list 4 5))
(assert (equalp *x* '(1 2 3 4 5)))

;; use the following idiom when working with recycling functions, especially
;; in conjunction with structural sharing.

(defun upto (max)
  (let ((result nil))
    (dotimes (i max)
      (push i result))
    (nreverse result)))

(assert (equalp (upto 10) '(0 1 2 3 4 5 6 7 8 9)))

(defun sort-numbers (numbers)
  (sort numbers #'<))

(defparameter *numbers* '(1 2 -19 3 2 1 5 -19 0))

(setf *numbers* (sort-numbers *numbers*))
(assert (equal *numbers* '(-19 -19 0 1 1 2 2 3 5)))

;; list manipulation functions

(assert (= (caar (list (list 1 2) 3)) 1))
(assert (equal (cadr (list (list 1 2) (list 3 4))) '(3 4)))
(assert (= (caadr (list (list 1 2) (list 3 4))) 3))

;;; the list mapping functions

(assert (equalp (map 'vector #'(lambda (x) (* x x)) #(1 2 3 4 5))
		#(1 4 9 16 25)))
(assert (equalp (mapcar #'(lambda (x) (* x x)) '(1 2 3 4 5))
		'(1 4 9 16 25)))
(assert (equalp (map 'vector #'+ #(1 2 3) #(4 5 6 7))
		#(5 7 9)))
(assert (equalp (map 'list #'+ '(1 2 3) '(4 5 6))
		'(5 7 9)))






