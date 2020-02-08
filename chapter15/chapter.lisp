;;;; Chapter 15 - Practical: A portable Pathname library

;;; read-time conditionalisation

;; :allegro, :sbcl, :cmu, :clisp et al
(defun print-message-for-platform ()
  #+sbcl (format t "Yes, we are on SBCL")
  #-sbcl (format t "This message will not be seen on SBCL"))







