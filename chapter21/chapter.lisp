;;;; Chapter 21 - Programming in the Large: Packages and Symbols.

;;; the three standard packages are: common-lisp. common-lisp-user, and keyword.

;;; defnining custom packages

(in-package #:common-lisp-user)

(defun hello-world ()
  "In the cl-user package."
  (format t "Hello, world~%"))

(defpackage #:com-acme-email
  (:use #:common-lisp)
  (:export #:parse-email-address
	   #:save
	   #:store))

(defpackage #:com-acme-text
  (:use #:common-lisp)
  (:export #:build-index
	   #:store-index
	   #:save-index
	   #:save))

(defpackage #:com-gigamonkeys-email-db
  (:use #:common-lisp
	#:com-gigamonkeys-text-db
	#:com-acme-text)
  (:import-from #:com-acme-email #:parse-email-address)
  (:shadow #:build-index)
  (:shadowing-import-from #:com-gigamonkeys-text-db #:save))

(in-package #:com-gigamonkeys-email-db)

(defun hello-world ()
  "In the com-gigamonkeys-email-db package."
  (format t "Hello, world~%"))

(in-package #:common-lisp-user)

;; this can also be done, but the strings have to be in all uppercase
(defpackage "COM-GIGAMONKEYS-EMAIL-DB-HELPER"
  (:use "COMMON-LISP"))

(in-package #:common-lisp-user)

;;; packaging reusable libraries

(defpackage #:com-gigamonkeys-text-db
  (:use #:common-lisp)
  (:export #:open-db
	   #:save
	   #:store))


