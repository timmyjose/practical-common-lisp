;;;; Chapter 19 - Beyond Exception Handling: Conditions and Restarts.

(define-condition malformed-log-entry-error (error)
  ((text
    :initarg :text
    :reader text)))

(defclass log-entry ()
  ((text
    :initarg :text
    :accessor text)))

(defmethod print-object ((object log-entry) stream)
  (format stream "~a { text: ~a }" (type-of object) (text object)))

(defclass malformed-log-entry (log-entry) ())

(defun well-formed-log-entry-p (text)
  (>= (length text) 10))

;; (defun parse-log-entry (text)
;;   (if (well-formed-log-entry-p text)
;;       (make-instance 'log-entry :text text)
;;       (error 'malformed-log-entry-error :text text)))

;; simply handling an error condition without restarting

;; (defun parse-log-file (file)
;;   (with-open-file (in file
;; 		      :direction :input)
;;     (loop for text = (read-line in nil nil) while text
;;        for entry = (handler-case (parse-log-entry text)
;; 		     (malformed-log-entry-error () nil))
;;        when entry collect it)))

;;; restarts

(defun parse-log-file (file)
  (with-open-file (in file
		      :direction :input)
    (loop for text = (read-line in nil nil) while text
       for entry = (restart-case (parse-log-entry text)
		     (skip-log-entry () nil))
       when entry collect it)))

;; (defun log-analyzer ()
;;   (let ((logs '(#p "sample-log-file")))
;;     (dolist (log-file logs)
;;       (analyze-log-file log))))

(defun analyze-log-file (log-file)
  (dolist (entry (parse-log-file log-file))
    (analyze-entry entry)))

(defun analyze-entry (entry)
  (format t "~a~%" entry))

(defun skip-log-entry (c)
  (declare (ignore c))
  (let ((restart (find-restart 'skip-log-entry)))
    (when restart (invoke-restart 'skip-log-entry))))

;; (defun log-analyzer ()
;;   (handler-bind ((malformed-log-entry-error #'skip-log-entry))
;;     (let ((logs '(#p "sample-log-file")))
;;       (dolist (log-file logs)
;; 	(analyze-log-file log-file)))))

;; restarting strategies are placed in the low-level code
(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
      (make-instance 'log-entry :text text)
      (restart-case (error 'malformed-log-entry-error :text text)
	(use-value (value) value)
	(reparse-entry (fixed-text) (parse-log-entry fixed-text)))))

(defvar *restart-chooser* 0)

(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error
		  #'(lambda (c)
		      (incf *restart-chooser*)
		      (cond
		      ((oddp *restart-chooser*) (use-value
						  (make-instance 'malformed-log-entry :text (text c))))
		      (t (invoke-restart 'reparse-entry "this is fill-in text"))))))
    (dolist (log-file '(#p "sample-log-file"))
      (analyze-log-file log-file))))
