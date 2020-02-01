;;; Chapter 17 - Object Reorientation: Classes

;; this has unbound slots
(defclass simple-class ()
  (foo
   bar))

(defun simple-class-example ()
  (let ((sc (make-instance 'simple-class)))
    (setf (slot-value sc 'foo) "foo")
    (setf (slot-value sc 'bar) "bar")
    (format t "~a, ~a~%" (slot-value sc 'foo) (slot-value sc 'bar))))

(defgeneric draw (shape)
  (:documentation "Draw the given shape."))

(defclass shape () ())

(defclass circle (shape)
  ((radius
    :initarg :radius
    :initform 0)))

(defclass rectangle (shape)
  ((length
    :initarg :length
    :initform 0)
   (breadth
    :initarg :breadth
    :initform 0)))

(defclass triangle (shape)
  ((base
    :initarg :base
    :initform 0)
   (height
    :initarg :height
    :initform 0)))

(defmethod draw ((shape circle))
  (format t "Drawing a circle of radius ~f~%" (slot-value shape 'radius)))

(defmethod draw ((shape rectangle))
  (format t "Drawing a rectangle of length ~d and breadth ~d~%"
	  (slot-value shape 'length)
	  (slot-value shape 'breadth)))

(defmethod draw ((shape triangle))
  (format t "Drawing a triangle of base ~d and height ~d~%"
	  (slot-value shape 'base)
	  (slot-value shape 'height)))

(defun basic-shape-example ()
  (let ((c (make-instance 'circle :radius 10))
	(r (make-instance 'rectangle :length 20 :breadth 10))
	(tr (make-instance 'triangle :base 10 :height 20)))
    (draw c)
    (draw r)
    (draw tr)))

;;; bank account example

(defvar *account-number* 0)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply the customer's name.")
    :accessor customer-name
    :documentation "The name of the customer")
   (balance
    :initarg :balance
    :initform 0
    :accessor balance
    :documentation "Current balance")
   (account-number
    :initform (incf *account-number*)
    :reader account-number
    :documentation "Account number, unique within a bank")
   (account-type
    :reader account-type
    :documentation "Type of account: gold, silver, or bronze")))

;; note that initform cannot access the slots, so this :after auxiliary method is necessary
(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
	  (cond
	    ((>= balance 100000) :gold)
	    ((>= balance 50000) :silve)
	    (t :bronze)))))

(defmethod initialize-instance :after ((account bank-account) &key opening-bonus-percentage)
  (when opening-bonus-percentage
    (incf (slot-value account 'balance)
	  (* (slot-value account 'balance) (/ opening-bonus-percentage 100)))))

(defun basic-bank-account-example ()
  (let ((bob (make-instance 'bank-account
			    :customer-name "Bob"
			    :balance 960000
			    :opening-bonus-percentage 5)))
    (format t "Name: ~a, Balance: ~d~%"
	    (customer-name bob)
	    (balance bob))))

(defvar *minimum-allowed-balance* 50000)

;; (defmethod assess-low-balance-penalty ((account bank-account))
;;   (with-slots (balance) account
;;     (when (< balance *minimum-allowed-balance*)
;;       (decf balance (* 0.01 balance)))))

;; (defmethod assess-low-balance-penalty ((account bank-account))
;;   (with-slots ((bal balance)) account
;;     (when (< bal *minimum-allowed-balance*)
;;       (decf bal (* 0.01 bal)))))

;; note that this method does not have an explicitly defined generic function - in this case,
;; the generic function is "inferred"
(defmethod assess-low-balance-penalty ((account bank-account))
  (with-accessors ((balance balance)) account
    (when (< balance *minimum-allowed-balance*)
      (decf balance (* 0.01 balance)))))

;; explicitly defined generic function

(defgeneric merge-accounts (bank-account1 bank-account2)
  (:documentation "Merge the two given accounts together, transferring the balance from the second account into the first."))

(defmethod merge-accounts ((account1 bank-account) (account2 bank-account))
  (with-accessors ((balance1 balance)) account1
    (with-accessors ((balance2 balance)) account2
      (incf balance1 balance2)
      (setf balance2 0))))







