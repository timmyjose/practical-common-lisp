;;;; Chapter 16 - Object Orientation: Generic Functions.

(defgeneric draw (shape)
  (:documentation "Draw the given shape on the screen."))

(defmethod draw ((shape circle))
  (format t "Drawing a circle of radius ~f~%" (radius cirle)))

(defmethod draw ((shape triangle))
  (format t "Drawing a triangle with base ~d and height ~d~%" (base triangle) (height triangle)))

(defgeneric withdraw (account amount)
  (:documentation "Withdraw the specified amount from the account. Signal an error if the current balance is less than the amount."))

(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Insufficient funds."))
  (decf (balance account) amount))

(defmethod withdraw ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))

(defmethod withdraw ((account proxy-account) amount)
  (withdraw (proxied-account proxy-account) amount))

;; matches a particular object using eql
(defmethod withdraw ((account (eql account *bank-president-account*)) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft))))
  (call-next-method))

;;; auxiliary method types - :before, :after, :around

(defmethod withdraw :before ((account bank-account) amount))

(defmethod withdraw :before ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account) amount)
      (incf (balance account) overdraft))))

;;; simple method combination types - +, ADD, OR, NCONC, PROGN, LIST, APPEND, MIN, and MAX.

(defgeneric priority (job)
  (:documentation "Return the priority at which the job must be run.")
  (:method-combination +))

(defmethod priority + ((job express-job)) 10)

(defgeneric combo-priority (job)
  (:documentation "Return the priority at which the job must be run.")
  (:method-combination + :most-specific-last))

;;;; multimethods - dispatching/specialising on more than one parameter

(defgeneric beat (drum stick)
  (:documentation "Produce a sound by hitting the given drum with the given stick."))

(defmethod beat ((drum snare-drum) (stick wooden-drumstick)))
(defmethod beat ((drum snare-drum) (stick brush)))
(defmethod beat ((drum snare-drum) (stick soft-mallet)))
(defmethod beat ((drum tom-tom) ((stick wooden-stick))))
(defmethod beat ((drum tom-tom) (stick brush)))
(defmethod beat ((drum tom-tom) (stick soft-mallet)))








