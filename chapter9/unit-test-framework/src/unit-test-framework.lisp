(in-package #:com-tzj-unit-test-framework)

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  `(combine-results
     ,@ (loop for f in forms collecting `(report-result ,f ',f))))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@ (loop for f in forms collecting `(unless ,f (setf ,result nil)))
	  ,result)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collecting `(,n (gensym)))
     ,@body))
